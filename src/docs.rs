use termion::{style, terminal_size};
use std::io;
use std::io::prelude::*;
use data::*;

// TODO: add Markdown support
/// Formatting settings for rendering documentation
pub struct DocFormat {
    /// Wrap words on terminal width
    pub word_wrap: bool,

    /// Terminal width
    /// 
    /// If given, the renderer will try not to exceed this in the output.
    pub width: Option<usize>,

    /// Whether to enable ANSI formatting codes
    /// 
    /// This has no effect when rendering to a string
    pub format_codes: bool
}

/// Wrap the given text to a specified maximum column
fn word_wrap(w: usize, text: &str) -> String {
    let line_max = w;
    let mut r = String::with_capacity(text.len());

    // perform word wrapping
    let mut words = Vec::with_capacity(w);
    let mut line_len = 0;
    for w in text.split(' ') {
        if line_len + w.len() >= line_max {
            r.push_str(&words.join(" "));
            r.push('\n');
            words.truncate(0);
            line_len = 0;
        }

        line_len += w.len() + 1;
        if w.contains('\n') {
            // flush line buffer and split the word on the newline boundary
            let newline_idx = w.rfind('\n').unwrap();
            let to_print = &w[..newline_idx+1];
            words.push(to_print);
            r.push_str(&words.join(" "));
            words.truncate(0);
            if newline_idx < w.len()-1 {
                words.push(&w[newline_idx+1..])
            }
            line_len = w.len() - newline_idx - 1;
        } else {
            words.push(w);
        }
    }
    if !words.is_empty() {
        r.push_str(&words.join(" "));
    }

    r
}

impl DocFormat {
    /// Create a new `DocFormat` initialized to sensible defaults
    pub fn new() -> Self {
        DocFormat {
            word_wrap: true,
            width: Some(terminal_size()
                        .unwrap_or((80,0)).0 as usize),
            format_codes: true
        }
    }

    /// Disable word wrapping
    pub fn no_wrap(&mut self) -> &mut Self {
        self.word_wrap = false;
        self
    }

    /// Enable word wrapping
    pub fn wrap(&mut self) -> &mut Self {
        self.word_wrap = true;
        self
    }

    /// Set whether to use format codes
    pub fn format(&mut self, e: bool) -> &mut Self {
        self.word_wrap = e;
        self
    }

    /// Unset the rendering target width
    pub fn no_width(&mut self) -> &mut Self {
        self.width = None;
        self
    }

    /// Set the rendering target width
    pub fn width(&mut self, w: usize) -> &mut Self {
        self.width = Some(w);
        self
    }

    /// Render a documentation object to a string
    pub fn render_str(&self, doc: &Documentation) -> String {
        unimplemented!()
    }

    /// Render a documentation object to an output stream
    pub fn render<W: Write>(&self,
                            doc: &Documentation,
                            out: &mut W) -> io::Result<()> {
        if let Some(o) = doc.origin {
            if let Some(w) = self.width {
                // render origin centered
                writeln!(out, "{:^width$}\n", o, width=w)?;
            } else {
                writeln!(out, "{}\n", o)?;
            }
        }

        if self.format_codes { write!(out, "{}", style::Underline)?; }
        for f in doc.forms.iter() {
            writeln!(out, "({})", f.join(" "))?;
        }
        if self.format_codes {
            write!(out, "{}", style::NoUnderline)?;
        }
        writeln!(out, "")?;

        if let Some(s) = doc.short_desc {
            if self.format_codes {
                write!(out, "{}", style::Bold)?;
            }
            if let Some(w) = self.width {
                writeln!(out, "{}\n", word_wrap(w, s))?;
            } else {
                writeln!(out, "{}\n", s)?;
            }
        }

        if let Some(d) = doc.description {
            if self.format_codes {
                write!(out, "{}", style::Reset)?;
            }
            if let Some(w) = self.width {
                let s = word_wrap(w, d);
                writeln!(out, "{}", s)?;
            } else {
                writeln!(out, "{}", d)?;
            }
        }

        Ok(())
    }
}

impl Default for DocFormat {
    fn default() -> Self {
        DocFormat::new()
    }
}
