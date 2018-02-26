use std::io;
use std::sync::Arc;
use std::io::prelude::*;

use termion::*;

use editor::{LineEditor, EditingDiscipline};
use completion::{EntryType, CompletionSet};

/// Line input renderer
///
/// This is responsible for rendering the basic shell prompt
pub struct LineRenderer<W: Write> {
    output: raw::RawTerminal<W>,
}

impl<W: Write> LineRenderer<W> {
    pub fn new(output: raw::RawTerminal<W>) -> Self {
        LineRenderer { output }
    }

    pub fn render<E: EditingDiscipline>(&mut self, editor: &LineEditor<E>)
            -> io::Result<()> {
        let buf = editor.buf();
        let s = buf.as_string();
        write!(self.output, "\r{}$ {}\r{}",
               clear::CurrentLine, s,
               cursor::Right(2+buf.cursor() as u16))?;
        self.output.flush()
    }

    pub fn into_output(self) -> raw::RawTerminal<W> { self.output }

    /// Get the output stream temporarily
    pub fn output(&mut self) -> &mut raw::RawTerminal<W> {&mut self.output}
}

/// Completion renderer
///
/// This handles rendering for the interactive completion functionality
pub struct CompleteRenderer<W: Write> {
    output: raw::RawTerminal<W>,
    set: CompletionSet
}

impl<W: Write> CompleteRenderer<W> {
    pub fn new(output: raw::RawTerminal<W>, set: &CompletionSet) -> Self {
        CompleteRenderer {
            output,
            set: set.to_owned()
        }
    }

    pub fn render<E: EditingDiscipline>(&mut self,
                  editor: &LineEditor<E>) -> io::Result<()> {
        let buf = editor.buf();
        let s = buf.as_string();
        let mark = self.set.marked_idx();
        let marked_entry = self.set.marked();

        // figure out what to render after the input string
        let after_input = match self.set.marked() {
            None => String::from(""),
            Some(e) => e.text.trim_left_matches(&s).to_owned()
        };

        // draw prompt
        write!(self.output, "\r{}$ {}{}{}{}{}\r", clear::CurrentLine, s,
               style::Italic, after_input, style::NoItalic,
               clear::AfterCursor)?;

        // draw completion list, capped to fixed size
        let nlines = self.set.len().min(if let Some(mark) = mark {
            // omit last line if mark is beyond range, so we have room to
            // draw the marked entry
            if mark > 10 { 9 }
            else { 10 }
        } else { 10 });

        // figure out where to put the separator for docstrings
        let max_len = self.set.entries().into_iter().take(nlines)
                          .map(|x| x.text.len())
                          .max()
                          .unwrap_or(1);
        let sep_column = max_len + 2;

        for comp in self.set.entries().into_iter().take(nlines) {
            let spaces = sep_column - comp.text.len();
            // set color based on type
            // TODO: make these configurable
            match comp.what {
                EntryType::SystemCommand =>
                    write!(self.output, "{}", color::Fg(color::Green))?,
                EntryType::FunctionBinding =>
                    write!(self.output, "{}", color::Fg(color::Cyan))?,
                EntryType::VariableBinding =>
                    write!(self.output, "{}", color::Fg(color::Yellow))?,
                EntryType::OtherForm =>
                    write!(self.output, "{}", color::Fg(color::Red))?,
                EntryType::File => {},
            }

            if marked_entry.as_ref()
                           .map(|e| Arc::ptr_eq(e, comp))
                           .unwrap_or(false) {
                // marked - highlight it
                write!(self.output, "\n{}{}{}",
                       style::Invert, comp.text, style::NoInvert)?;
            } else {
                write!(self.output, "\n{}", comp.text)?;
            }

            // write docs if available
            if let Some(ref doc) = comp.docs {
                write!(self.output, "{}- {}", " ".repeat(spaces), doc)?;
            }
            write!(self.output, "{}\r", color::Fg(color::Reset));
        }

        // move back to start
        write!(self.output, "{}\r{}",
               cursor::Up(nlines as u16),
               cursor::Right(2+buf.cursor() as u16))?;
        self.output.flush()
    }

    pub fn end(mut self) -> (raw::RawTerminal<W>, io::Result<()>) {
        // clear completions
        let r = write!(self.output, "\r{}", clear::AfterCursor);
        (self.output, r)
    }

    /// Update the active set of completions
    ///
    /// The marked element will be used as the selected one, if available
    pub fn update(&mut self, set: &CompletionSet) {
        self.set = set.to_owned();
    }

    /// Get the output stream temporarily
    pub fn output(&mut self) -> &mut raw::RawTerminal<W> {&mut self.output}
}

/// History search renderer
///
/// This handles rendering for the interactive reverse history search (Ctrl-R)
/// functionality
pub struct HistoryRenderer<W: Write> {
    output: raw::RawTerminal<W>,
}

pub enum Renderer<W: Write> {
    Line(LineRenderer<W>),
    Complete(CompleteRenderer<W>),
    //History(HistoryRenderer<W>)
}

impl<W: Write> Renderer<W> {
    pub fn new(write: raw::RawTerminal<W>) -> Self {
        Renderer::Line(LineRenderer {output: write})
    }

    /// Convert to a line renderer
    ///
    /// Return any I/O errors that occurred during conversion. If this happens,
    /// the terminal might be in a weird state. Use caution.
    pub fn to_line(self) -> (Self, io::Result<()>) {
        match self {
            Renderer::Line(r) => (Renderer::Line(r), Ok(())),
            Renderer::Complete(r) => {
                let (r,e) = r.end();
                (Renderer::Line(LineRenderer {output: r}), e)
            },
        }
    }

    /// Try to convert to a completion renderer
    ///
    /// Upon failure, return the original version and the error that occurred
    pub fn to_complete(self, set: &CompletionSet) -> Result<Self, (Self, io::Error)> {
        match self {
            Renderer::Line(r) => Ok(Renderer::Complete(
                    CompleteRenderer::new(r.into_output(), set))),
            Renderer::Complete(r) => Ok(Renderer::Complete(r))
        }
    }

    pub fn output(&mut self) -> &mut raw::RawTerminal<W> {
        match *self {
            Renderer::Line(ref mut r) => &mut r.output,
            Renderer::Complete(ref mut r) => &mut r.output
        }
    }

    pub fn render<E: EditingDiscipline>(&mut self,
                  editor: &LineEditor<E>) -> io::Result<()> {
        match *self {
            Renderer::Line(ref mut r) => r.render(editor),
            Renderer::Complete(ref mut r) => r.render(editor),
        }
    }
}
