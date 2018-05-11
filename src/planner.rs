//! # Pipeline Planning Engine
//!
//! The pipeline planning engine converts a pipeline into a plan for how to
//! assemble and execute it. This conversion operation does not require
//! interaction with the OS. Similarly, the results of a conversion can be
//! executed without directly evaluating any functions.

use std::path::PathBuf;

use data::*;
use evaluate::find_command;
use environment::{global, empty, run_fn};

#[derive(Debug)]
pub enum PlanningError {
    MultipleInputs,
    MultipleOutputs,
    NotFound,
    Evaluation(EvalError),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
/// Data type for the result of a pipeline element
enum PipeType { Stream, Object, Cap, Either }

impl PipeType {
    fn accepts(&self, input: &PipeType) -> bool {
        match (self, input) {
            (&PipeType::Stream, &PipeType::Stream) => true,
            (&PipeType::Object, &PipeType::Object) => true,
            (&PipeType::Either, &PipeType::Stream) => true,
            (&PipeType::Either, &PipeType::Object) => true,
            (_,                 &PipeType::Either) => true,
            _                                      => false,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
/// Representation of the conversion ops which can take place in a pipeline.
enum AdapterType {
    StreamToPoly,     // full semantic stream parsing
    StreamToString,   // stream to object conversion without any semantic stuff
    PolyToStream,     // object to stream conversion
    StreamDelim(char),// semantic parsing with specified delimiter
}

impl AdapterType {
    /// Get the input and output types for this adapter
    fn io_types(&self) -> (PipeType, PipeType) {
        match self {
            &AdapterType::StreamToPoly  => (PipeType::Stream, PipeType::Object),
            &AdapterType::StreamToString=> (PipeType::Stream, PipeType::Object),
            &AdapterType::PolyToStream  => (PipeType::Object, PipeType::Stream),
            &AdapterType::StreamDelim(_)=> (PipeType::Stream, PipeType::Object),
        }
    }

    /// Generate an adapter fitting a given I/O pair
    /// 
    /// # Panics
    /// This panics if adapting the two types is impossible
    fn fit(i: PipeType, o: PipeType) -> Self {
        match (i,o) {
            (PipeType::Stream, PipeType::Object) => AdapterType::StreamToPoly,
            (PipeType::Object, PipeType::Stream) => AdapterType::PolyToStream,
            _                                   => panic!("cannot adapt stream")
        }
    }
}

#[derive(Debug, PartialEq)]
/// One individual operations in a pipeline plan
enum PlanElement {
    Expression(Value),      // obj -> obj
    Command {               // stream -> stream
        exec: PathBuf,
        invoked_name: String,
        args: Vec<Value>
    },
    Adapter(AdapterType),   // a -> b
    FromFile(String),       // x -> stream
    FromVar(Identifier),    // x -> obj
    Stdin,                  // x -> stream
    Stdout,                 // obj/stream -> x (pretty-print if object)
    ToFile(String),         // stream -> x
    AppendFile(String),     // stream -> x
    IntoVar(Identifier),    // obj -> x
    AppendVar(Identifier),  // obj -> x
}

impl PlanElement {
    fn io_types(&self) -> (PipeType, PipeType) {
        match self {
            &PlanElement::Expression(_)  => (PipeType::Object, PipeType::Object),
            &PlanElement::Command{..}    => (PipeType::Stream, PipeType::Stream),
            &PlanElement::Adapter(ref a) => a.io_types(),
            &PlanElement::FromFile(_)    => (PipeType::Cap,    PipeType::Stream),
            &PlanElement::FromVar(_)     => (PipeType::Cap,    PipeType::Object),
            &PlanElement::Stdin          => (PipeType::Cap,    PipeType::Stream),
            &PlanElement::Stdout         => (PipeType::Either, PipeType::Cap),
            &PlanElement::ToFile(_)      => (PipeType::Stream, PipeType::Cap),
            &PlanElement::AppendFile(_)  => (PipeType::Stream, PipeType::Cap),
            &PlanElement::IntoVar(_)     => (PipeType::Object, PipeType::Cap),
            &PlanElement::AppendVar(_)   => (PipeType::Object, PipeType::Cap),
        }
    }

    /// Build an element adapting between the given types
    pub fn adapt(i: PipeType, o: PipeType) -> Self {
        PlanElement::Adapter(AdapterType::fit(i, o))
    }

    /// Build a plan element for a `Transformer`
    ///
    /// This converts a transform into a plan element using the following rules
    /// in order:
    ///
    /// 1. If the first element is a symbol with a bound value, and there's more
    ///    than one element, return the elements as a list.
    /// 2. If the first element is a symbol with a bound value, and it's the
    ///    only element, return the bound value.
    /// 3. If the first element can't be converted with `get_string`, return
    ///    either all elements as a list (if more than one) or the first.
    /// 4. Get the first element's string value using `get_string`, and try to
    ///    find a system command maching it. If successful, evaluate the
    ///    arguments and return a command element.
    /// 5. Fail
    ///
    /// # Panics
    /// This will panic if passed a transformer containing values which generate
    /// errors in response to any method other than `evaluate` or `execute`.
    pub fn build(mut xform: Transformer) -> Result<Self, PlanningError> {
        let first_sym = xform.0[0].get_symbol().wait().unwrap();
        if let Some(s) = first_sym {
            // try looking it up
            let r = global().get(&*(s.0));

            // if we find it, use that
            if let Some(r) = r {
                if xform.0.len() == 1 {
                    return Ok(PlanElement::Expression(r));
                } else {
                    return Ok(PlanElement::Expression(Value::list(xform.0)));
                }
            }
        }

        let first_str = xform.0[0].get_string().wait().unwrap();
        let cmd = if let Some(c) = first_str {c}
                  else {
                      if xform.0.len() == 1 {
                          return Ok(PlanElement::Expression(
                                  xform.0.swap_remove(0)));
                      } else {
                          return Ok(PlanElement::Expression(Value::list(xform.0)));
                      }
                  };

        // try looking up as a command
        let mut opts = if let Some(cmds) = find_command(&cmd) { cmds }
                       else {
                           eprintln!("ysh: failed to locate command");
                           return Err(PlanningError::NotFound);
                       };

        if opts.len() > 1 {
            eprintln!("ysh: more than one candidate for '{}'", cmd);
            return Err(PlanningError::NotFound);
        } else if opts.len() == 0 {
            eprintln!("ysh: command not found: {}", cmd);
            return Err(PlanningError::NotFound);
        } else {
            Ok(PlanElement::Command {
                exec: opts.pop().unwrap(),
                invoked_name: cmd,
                args: xform.0.split_off(1)
            })
        }
    }
}

#[derive(Debug)]
/// Builder for a pipeline execution plan
///
/// This accumulates and generates a set of pipeline evaluation ops for the
/// given pipeline so that the resulting chain will perform as few conversions
/// as possible.
///
/// As a side effect of this planning process, we can accomodate commands that
/// need to be plugged directly into stdin/stdout like `vim` or `htop`.
/// 
/// You should only need this object to generate plans procedurally. For simple
/// cases, `Plan::build` should be sufficient.
///
/// To use this object, create an empty planner with `new`, then add each
/// component using the `push` and `add_terminal` methods. When done, call
/// `compile` to get a `Plan` object. Once frozen, the pipeline can be used to
/// launch instances of the described job.
pub struct Planner {
    /// The inner components of the plan which don't directly link to the input
    /// or output
    elems: Vec<PlanElement>,

    /// The plan's input element
    input: Option<PlanElement>,

    /// The plan's output element
    output: Option<PlanElement>,

    /// The current type at the head of the pipeline
    ///
    /// Set to `Either` by default; when finalizing, elements will be inserted
    /// to convert the input type into the first element's input.
    output_type: PipeType,
}

impl Planner {
    /// Create a new empty plan
    ///
    /// By default, this will use standard input as input to the pipeline and
    /// standard output as the output. To change this, apply terminal modes.
    pub fn new() -> Self {
        Planner {
            elems: Vec::new(),
            input: None,
            output: None,
            output_type: PipeType::Stream,
        }
    }

    /// Add adapter elements to the front of the plan in order to make the head
    /// compatible with the given target
    fn adapt_head(&mut self, tgt: PipeType) {
        if tgt.accepts(&self.output_type) { return; }

        let elem = PlanElement::adapt(self.output_type, tgt);
        self.output_type = elem.io_types().1;
        self.elems.push(elem);
    }

    /// Add a terminal (original input or final output) node to the pipeline
    ///
    /// If the required terminal point (i.e. input or output) has already been
    /// specified by another call, this function may fail with a corresponding
    /// error code.
    pub fn add_terminal(&mut self,
                        term: TerminalMode) -> Result<(), PlanningError> {
        let (f,l) = match term {
            TerminalMode::ReplaceFile(s) => (None, Some(PlanElement::ToFile(s))),
            TerminalMode::AppendFile(s) => (None, Some(PlanElement::AppendFile(s))),
            TerminalMode::SetVariable(id) => (None, Some(PlanElement::IntoVar(id))),
            TerminalMode::AppendVariable(id) => (None, Some(PlanElement::AppendVar(id))),
            TerminalMode::InputFile(s) => (Some(PlanElement::FromFile(s)), None),
            TerminalMode::InputVar(id) => (Some(PlanElement::FromVar(id)), None),
        };

        if let Some(l) = l { // it's an output mode
            if self.output.is_none() {
                self.output = Some(l);
                Ok(())
            } else {
                Err(PlanningError::MultipleOutputs)
            }
        } else if let Some(f) = f { // it's an input mode
            if !self.input.is_none() {
                return Err(PlanningError::MultipleInputs);
            }

            self.input = Some(f);
            Ok(())
        } else {
            panic!("cannot interpret terminal mode. this is a bug.")
        }
    }

    /// Add a pipeline component to the end of plan
    ///
    /// If the component doesn't have a link, it will default to `Pipe` if
    /// another component is added.
    ///
    /// This method will automatically insert adapter nodes to convert the
    /// current head into the appropriate type.
    pub fn push(&mut self, c: PipelineComponent) -> Result<(), PlanningError> {
        let PipelineComponent { xform, link } = c;

        let cmd = PlanElement::build(xform)?;
        let cmd_io = cmd.io_types();

        // add the element to the plan
        self.adapt_head(cmd_io.0);
        self.elems.push(cmd);
        self.output_type = cmd_io.1;

        // tack on the requested pipe
        if let Some(link) = link {
            let elem = match link {
                PipeMode::DelimitedPipe(c) =>
                    Some(PlanElement::Adapter(AdapterType::StreamDelim(c))),
                PipeMode::PipeText =>
                    Some(PlanElement::Adapter(AdapterType::StreamToString)),
                PipeMode::Pipe => None
            };

            if let Some(e) = elem {
                // don't add an adapter if it wouldn't help
                if e.io_types().1 != self.output_type {
                    self.adapt_head(e.io_types().0);
                    self.output_type = e.io_types().1;
                    self.elems.push(e);
                }
            }
        }

        Ok(())
    }

    /// Compile the plan
    ///
    /// This consumes the planner and builds a compiled pipeline which can be
    /// used to launch jobs.
    ///
    /// # Panics
    /// This will panic if run on a plan which contains no elements.
    pub fn compile(mut self) -> Result<Plan, PlanningError> {
        // generate adapter sequences for inputs and outputs
        let input = self.input.take().unwrap_or(PlanElement::Stdin);
        let output = self.output.take().unwrap_or(PlanElement::Stdout);

        let mut plan = Plan { steps: Vec::new() };

        if self.elems.is_empty() {
            // special case - we can't rely on io_types normally here
            panic!("attempted to compile empty plan");
        }

        // initialize the pipeline with the stream's input element
        let mut eval_group = None;
        match input {
            PlanElement::FromFile(pth) =>
                plan.steps.push(PlanStep::FromFile(PathBuf::from(pth))),
            PlanElement::FromVar(ident) => {
                // infer the output mode from the first element
                let otype = self.elems[0].io_types().0;
                let adapter = AdapterType::fit(PipeType::Object, otype);
                eval_group = Some((
                    vec![Value::from(ident)],
                    EvalInputType::NoInput,
                    EvalOutputType::from(adapter),
                ));
            }
            PlanElement::Stdin => plan.steps.push(PlanStep::FromStdin),
            _ => panic!("nonsense planner input element")
        }

        // start loading stuff into the pipeline
        let mut is_output_object = false; // needed for intovar/appendvar
        for elem in self.elems {
            match elem {
                PlanElement::Expression(val) => {
                    if eval_group.is_none() {
                        eval_group = Some((vec![],
                                           EvalInputType::PolyObject,
                                           EvalOutputType::IntoStr));
                    }
                    eval_group.as_mut().unwrap().0.push(val);
                    is_output_object = true;
                },
                PlanElement::Command {exec, invoked_name, args} => {
                    is_output_object = false;
                    // if we're in an eval group, finish it
                    if let Some(group) = eval_group.take() {
                        plan.steps.push(PlanStep::EvalGroup {
                            body: group.0,
                            in_type: group.1,
                            out_type: group.2
                        });
                    }

                    // perform full argument expansion
                    let args = args.into_iter()
                        .map(|a| a.evaluate(&empty()))
                        .collect::<Eval<Vec<_>>>().wait()
                        .and_then(|a| a.into_iter()
                                       .map(|x| match &x.data {
                                           // don't expand fns or macros
                                           &ValueData::Function(_) =>
                                               x.name.clone()
                                                .map(|x| Value::from(x))
                                                .unwrap_or(x),
                                           &ValueData::Macro(_) =>
                                               x.name.clone()
                                                .map(|x| Value::from(x))
                                                .unwrap_or(x),
                                           _ => x })
                                       .map(|x| match &x.data {
                                           // glob expansion for symbol/string
                                           &ValueData::Symbol(_) =>
                                               run_fn("fs/glob", &[x.clone()])
                                              .map(|x| x.wait())
                                              .unwrap_or(Ok(x.clone()))
                                              .map(|l| if l.into_seq().wait()
                                                           .unwrap().len() == 0 {
                                                  x.clone()} else {l}),
                                           &ValueData::Str(_) =>
                                               run_fn("fs/glob", &[x.clone()])
                                              .map(|x| x.wait())
                                              .unwrap_or(Ok(x.clone()))
                                              .map(|l| if l.into_seq().wait()
                                                           .unwrap().len() == 0 {
                                                  x.clone()} else {l}),
                                           _ => Ok(x) })
                                       .collect::<Result<Vec<Value>, _>>())
                        .and_then(|a| a.into_iter()
                                       .map(|x| x.into_args())
                                       .collect::<Eval<Vec<_>>>()
                                       .wait())
                        .map(|a| a.into_iter()
                                  .flat_map(|x| x)
                                  .collect::<Vec<_>>());
                    
                    let args = match args {
                        Ok(r) => r,
                        Err(e) => return Err(PlanningError::Evaluation(e))
                    };

                    plan.steps.push(PlanStep::Command {
                        exec, invoked_name,
                        args: args.into_iter().map(|x| x.into_bytes()).collect()
                    });
                },
                PlanElement::Adapter(t) => {
                    // This should only happen on either an evalgroup/evalgroup
                    // boundary or a command/evalgroup boundary.
                    //
                    // If the last step is a command, we can just initialize a
                    // new evalgroup here. If we're already in an evalgroup,
                    // then the next is either a command (in which case we can
                    // just configure the group's output mode) or another
                    // evalgroup element (in which case we need to convert to
                    // text and then back).
                    //
                    // We store the most recent adapter in the evalgroup's
                    // output mode.
                    //
                    // The evalgroup/evalgroup case isn't really ready yet, so
                    // just ignore it for now and treat the adapter like it's
                    // not there.

                    if eval_group.is_some() {
                        // we're in an evalgroup - assume that the next will be
                        // a plan, and replace the group's output mode
                        eval_group.as_mut().unwrap().2 = EvalOutputType::from(t);
                    } else if let Some(&PlanStep::Command {..}) = plan.steps.last() {
                        // we're doing 'cmd | evalgroup' - create a new group
                        // and set up its input type
                        eval_group = Some((
                            Vec::new(),
                            EvalInputType::from(t),
                            EvalOutputType::Object,
                        ));
                    }

                    is_output_object = t.io_types().1 == PipeType::Object;
                    
                    // TODO: implement evalgroup/evalgroup adaptation rules
                    // TODO: validate evalgroup/evalgroup ^ evalgroup/cmd rule
                    //       when processing command -- an empty evalgroup should
                    //       cause a panic
                },

                // other ones are terminals, which make no sense in the middle
                // of a planning list
                _ => panic!("found nonsense element in planner body")
            }
        }

        // finish pending evalgroup
        if let Some(group) = eval_group {
            plan.steps.push(PlanStep::EvalGroup {
                body: group.0,
                in_type: group.1,
                out_type: group.2
            });
            is_output_object = true;
        }

        // handle ending element
        match output {
            PlanElement::Stdout => {
                {
                    let last = plan.steps.last_mut();
                    if let Some(&mut PlanStep::EvalGroup {ref mut out_type, ..}) = last {
                        // pretty-print if we're sending to stdout
                        *out_type = EvalOutputType::PrettyPrint;
                    }
                }
                plan.steps.push(PlanStep::ToStdout);
            },
            PlanElement::ToFile(fname) => {
                {
                    let last = plan.steps.last_mut();
                    if let Some(&mut PlanStep::EvalGroup {ref mut out_type, ..}) = last {
                        // pretty-print if we're sending to stdout
                        *out_type = EvalOutputType::PrettyPrint;
                    }
                }
                plan.steps.push(PlanStep::ToFile {
                    path: PathBuf::from(fname),
                    replace: true
                });
            },
            PlanElement::AppendFile(fname) => {
                {
                    let last = plan.steps.last_mut();
                    if let Some(&mut PlanStep::EvalGroup {ref mut out_type, ..}) = last {
                        // pretty-print if we're sending to stdout
                        *out_type = EvalOutputType::PrettyPrint;
                    }
                }
                plan.steps.push(PlanStep::ToFile {
                    path: PathBuf::from(fname),
                    replace: false
                });
            },
            PlanElement::IntoVar(id) => {
                {
                    if !is_output_object {
                        // We need to adapt the command stream into an object.
                        // Since this is a special case, just use an empty
                        // evalgroup to denote the conversion.
                        plan.steps.push(PlanStep::EvalGroup {
                            body: vec![],
                            in_type: EvalInputType::PolyObject,
                            out_type: EvalOutputType::Object
                        });
                    } else if let Some(&mut PlanStep::EvalGroup {ref mut out_type,..})
                            = plan.steps.last_mut() {
                        // make sure the evalgroup is using the right output mode
                        *out_type = EvalOutputType::Object;
                    }
                }
                plan.steps.push(PlanStep::ToVar {
                    ident: id,
                    replace: true
                });
            },
            PlanElement::AppendVar(id) => {
                {
                    if !is_output_object {
                        // We need to adapt the command stream into an object.
                        // Since this is a special case, just use an empty
                        // evalgroup to denote the conversion.
                        plan.steps.push(PlanStep::EvalGroup {
                            body: vec![],
                            in_type: EvalInputType::PolyObject,
                            out_type: EvalOutputType::Object
                        });
                    } else if let Some(&mut PlanStep::EvalGroup {ref mut out_type,..})
                            = plan.steps.last_mut() {
                        // make sure the evalgroup is using the right output mode
                        *out_type = EvalOutputType::Object;
                    }
                }
                plan.steps.push(PlanStep::ToVar {
                    ident: id,
                    replace: false
                });
            },
            _ => panic!("invalid ending element found in planner")
        }

        Ok(plan)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
/// Input conversion mode for an `EvalGroup` plan step
pub enum EvalInputType {
    PolyObject,
    StreamString,
    StreamDelim(char),
    NoInput
}

impl From<AdapterType> for EvalInputType {
    fn from(t: AdapterType) -> Self {
        match t {
            AdapterType::StreamToPoly   => EvalInputType::PolyObject,
            AdapterType::StreamToString => EvalInputType::StreamString,
            AdapterType::StreamDelim(c) => EvalInputType::StreamDelim(c),
            _ => panic!("nonsense adapter type found")
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
/// Output target for an `EvalGroup` plan step
pub enum EvalOutputType {
    // convert using into_str() and write to output
    IntoStr,

    // pretty-print and write to output
    PrettyPrint,

    // emit as Lisp object - combines with variable storage target
    Object
}

impl From<AdapterType> for EvalOutputType {
    fn from(t: AdapterType) -> Self {
        match t {
            AdapterType::PolyToStream   => EvalOutputType::IntoStr,
            _ => panic!("nonsense adapter type found")
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
/// A single logical step in the execution of a compiled execution plan
pub enum PlanStep {
    FromStdin,
    FromFile(PathBuf),
    EvalGroup {
        in_type: EvalInputType,
        out_type: EvalOutputType,
        body: Vec<Value>,
    },
    Command {
        exec: PathBuf,
        invoked_name: String,
        args: Vec<Vec<u8>>
    },
    ToStdout,
    ToVar {
        ident: Identifier,
        replace: bool
    },
    ToFile {
        path: PathBuf,
        replace: bool
    }
}

#[derive(Debug, PartialEq)]
/// A compiled pipeline execution plan
///
/// Instead of the "flat" structure of a planner, this represents execution
/// plans as grouped collections of one or more operations.
pub struct Plan {
    steps: Vec<PlanStep>
}

impl Plan {
    /// Create a plan from the given pipeline
    ///
    /// The resulting plan will be frozen if generated successfully
    pub fn build(p: Pipeline) -> Result<Self, PlanningError> {
        let Pipeline {elements, terminals} = p;
        let mut plan = Planner::new();

        for t in terminals { plan.add_terminal(t)?; }
        for e in elements { plan.push(e)?; }

        plan.compile()
    }

    /// Get a reference to the list of steps
    pub fn steps(&self) -> &[PlanStep] {
        &self.steps
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use environment::*;
    use evaluate::Executable;
    use data::*;

    fn mock_shell_locate(env: &Environment, args: &[Value]) -> EvalResult {
        let a = args[0].into_str().wait()?;
        match &a[..] {
            "command-1" => Ok(Value::str("/test/command-1")),
            "command-2" => Ok(Value::str("/test/command-2")),
            "command-3" => Ok(Value::str("/test/command-3")),
            _           => Ok(Value::empty())
        }
    }

    fn mock_fs_glob(env: &Environment, args: &[Value]) -> EvalResult {
        let a = args[0].into_str().wait()?;
        match &a[..] {
            "*.foo"     => Ok(Value::list(vec![Value::str("glob1.foo"),
                                               Value::str("glob2.foo"),
                                               Value::str("glob3.foo")])),
            _ => Ok(Value::list(vec![args[0].clone()]))
        }
    }

    /// Configure environment for planning tests
    fn setup_plan_test_env() {
        let env = global();
        env.set("fs/glob", Value::from(Executable::native(mock_fs_glob)));
        env.set("shell/locate", Value::from(Executable::native(mock_shell_locate)));
        env.set("test-symbol", Value::from(Executable::native(
                    |_,_| Ok(Value::str("testvalue")))));
    }

    #[should_panic]
    #[test]
    fn planner_empty() {
        setup_plan_test_env();
        Planner::new().compile();
    }

    #[test]
    fn planner_single_commands() {
        setup_plan_test_env();

        {
            // generate a basic single-command plan
            let mut p = Planner::new();
            p.push(PipelineComponent {
                xform: Transformer(vec![Value::str("command-1")]),
                link: None
            }).expect("Failed to push pipeline component");

            // verify that it compiles properly
            let r = p.compile().expect("Failed to compile plan");
            assert_eq!(r, Plan {
                steps: vec![PlanStep::FromStdin, PlanStep::Command {
                    exec: PathBuf::from("/test/command-1"),
                    invoked_name: String::from("command-1"),
                    args: vec![]
                }, PlanStep::ToStdout]
            });
        }

        {
            // generate a basic single-command plan
            let mut p = Planner::new();
            p.push(PipelineComponent {
                xform: Transformer(vec![
                                   Value::str("command-2"),
                                   Value::from(Identifier::new("-abcd")),
                                   Value::from(Identifier::new("--other-flag")),
                                   Value::str("file")]),
                link: None
            }).expect("Failed to push pipeline component");

            // verify that it compiles properly
            let r = p.compile().expect("Failed to compile plan");
            assert_eq!(r, Plan {
                steps: vec![PlanStep::FromStdin, PlanStep::Command {
                    exec: PathBuf::from("/test/command-2"),
                    invoked_name: String::from("command-2"),
                    args: vec![b"-abcd"[..].to_owned(),
                               b"--other-flag"[..].to_owned(),
                               b"file"[..].to_owned(),]
                }, PlanStep::ToStdout]
            });
        }
    }

    #[test]
    fn planner_command_pipeline() {
        setup_plan_test_env();

        {
            let mut p = Planner::new();
            p.push(PipelineComponent {
                xform: Transformer(vec![Value::str("command-1")]),
                link: Some(PipeMode::Pipe)
            }).expect("Failed to push pipeline component");
            p.push(PipelineComponent {
                xform: Transformer(vec![Value::str("command-2"),
                                        Value::str("arg1")]),
                link: Some(PipeMode::Pipe)
            }).expect("Failed to push pipeline component");
            p.push(PipelineComponent {
                xform: Transformer(vec![Value::str("command-3"),
                                        Value::str("arg3")]),
                link: None
            }).expect("Failed to push pipeline component");

            let r = p.compile().expect("Failed to compile plan");
            assert_eq!(r, Plan {
                steps: vec![
                    PlanStep::FromStdin,
                    PlanStep::Command {
                        exec: PathBuf::from("/test/command-1"),
                        invoked_name: String::from("command-1"),
                        args: vec![]
                    },
                    PlanStep::Command {
                        exec: PathBuf::from("/test/command-2"),
                        invoked_name: String::from("command-2"),
                        args: vec![b"arg1"[..].to_owned()]
                    },
                    PlanStep::Command {
                        exec: PathBuf::from("/test/command-3"),
                        invoked_name: String::from("command-3"),
                        args: vec![b"arg3"[..].to_owned()]
                    },
                    PlanStep::ToStdout
                ]
            })
        }
    }

    #[test]
    fn planner_input_redirection() {
        setup_plan_test_env();

        {
            let mut p = Planner::new();
            p.push(PipelineComponent {
                xform: Transformer(vec![Value::str("command-1")]),
                link: None
            }).expect("Failed to push pipeline component");

            p.add_terminal(TerminalMode::InputFile(String::from("testfile")))
             .expect("Failed to add terminal");

            let r = p.compile().expect("Failed to compile plan");
            assert_eq!(r, Plan {
                steps: vec![
                    PlanStep::FromFile(PathBuf::from("testfile")),
                    PlanStep::Command {
                        exec: PathBuf::from("/test/command-1"),
                        invoked_name: String::from("command-1"),
                        args: vec![]
                    },
                    PlanStep::ToStdout
                ]
            });
        }
        {
            let mut p = Planner::new();
            p.push(PipelineComponent {
                xform: Transformer(vec![Value::str("command-1")]),
                link: None
            }).expect("Failed to push pipeline component");

            p.add_terminal(TerminalMode::InputVar(Identifier::new("test")))
             .expect("Failed to add terminal");

            let r = p.compile().expect("Failed to compile plan");
            assert_eq!(r, Plan {
                steps: vec![
                    PlanStep::EvalGroup {
                        in_type: EvalInputType::NoInput,
                        body: vec![Value::from(Identifier::new("test"))],
                        out_type: EvalOutputType::IntoStr,
                    },
                    PlanStep::Command {
                        exec: PathBuf::from("/test/command-1"),
                        invoked_name: String::from("command-1"),
                        args: vec![]
                    },
                    PlanStep::ToStdout
                ]
            });
        }
    }

    #[test]
    fn planner_output_redirection() {
        setup_plan_test_env();

        {
            let mut p = Planner::new();
            p.push(PipelineComponent {
                xform: Transformer(vec![Value::str("command-1")]),
                link: None
            }).expect("Failed to push pipeline component");

            p.add_terminal(TerminalMode::ReplaceFile(String::from("test")))
             .expect("Failed to add terminal");

            let r = p.compile().expect("Failed to compile plan");
            assert_eq!(r, Plan {
                steps: vec![
                    PlanStep::FromStdin,
                    PlanStep::Command {
                        exec: PathBuf::from("/test/command-1"),
                        invoked_name: String::from("command-1"),
                        args: vec![]
                    },
                    PlanStep::ToFile {
                        path: PathBuf::from("test"),
                        replace: true
                    }
                ]
            });
        }
        {
            let mut p = Planner::new();
            p.push(PipelineComponent {
                xform: Transformer(vec![Value::str("command-1")]),
                link: None
            }).expect("Failed to push pipeline component");

            p.add_terminal(TerminalMode::AppendFile(String::from("test")))
             .expect("Failed to add terminal");

            let r = p.compile().expect("Failed to compile plan");
            assert_eq!(r, Plan {
                steps: vec![
                    PlanStep::FromStdin,
                    PlanStep::Command {
                        exec: PathBuf::from("/test/command-1"),
                        invoked_name: String::from("command-1"),
                        args: vec![]
                    },
                    PlanStep::ToFile {
                        path: PathBuf::from("test"),
                        replace: false
                    }
                ]
            });
        }
        {
            let mut p = Planner::new();
            p.push(PipelineComponent {
                xform: Transformer(vec![Value::str("command-1")]),
                link: None
            }).expect("Failed to push pipeline component");

            p.add_terminal(TerminalMode::SetVariable(Identifier::new("test")))
             .expect("Failed to add terminal");

            let r = p.compile().expect("Failed to compile plan");
            assert_eq!(r, Plan {
                steps: vec![
                    PlanStep::FromStdin,
                    PlanStep::Command {
                        exec: PathBuf::from("/test/command-1"),
                        invoked_name: String::from("command-1"),
                        args: vec![]
                    },
                    PlanStep::EvalGroup {
                        in_type: EvalInputType::PolyObject,
                        out_type: EvalOutputType::Object,
                        body: vec![]
                    },
                    PlanStep::ToVar {
                        ident: Identifier::new("test"),
                        replace: true
                    }
                ]
            });
        }
        {
            let mut p = Planner::new();
            p.push(PipelineComponent {
                xform: Transformer(vec![Value::str("command-1")]),
                link: None
            }).expect("Failed to push pipeline component");

            p.add_terminal(TerminalMode::AppendVariable(Identifier::new("test")))
             .expect("Failed to add terminal");

            let r = p.compile().expect("Failed to compile plan");
            assert_eq!(r, Plan {
                steps: vec![
                    PlanStep::FromStdin,
                    PlanStep::Command {
                        exec: PathBuf::from("/test/command-1"),
                        invoked_name: String::from("command-1"),
                        args: vec![]
                    },
                    PlanStep::EvalGroup {
                        in_type: EvalInputType::PolyObject,
                        out_type: EvalOutputType::Object,
                        body: vec![]
                    },
                    PlanStep::ToVar {
                        ident: Identifier::new("test"),
                        replace: false
                    }
                ]
            });
        }
    }

    #[test]
    fn planner_globbing() {
        setup_plan_test_env();

        {
            let mut p = Planner::new();
            p.push(PipelineComponent {
                xform: Transformer(vec![Value::str("command-1"),
                                        Value::from(Identifier::new("*.foo"))]),
                link: None
            }).expect("Failed to push pipeline component");

            // verify that it compiles properly
            let r = p.compile().expect("Failed to compile plan");
            assert_eq!(r, Plan {
                steps: vec![PlanStep::FromStdin, PlanStep::Command {
                    exec: PathBuf::from("/test/command-1"),
                    invoked_name: String::from("command-1"),
                    args: vec![b"glob1.foo"[..].to_owned(),
                               b"glob2.foo"[..].to_owned(),
                               b"glob3.foo"[..].to_owned()]
                }, PlanStep::ToStdout]
            });
        }
    }

    #[test]
    fn planner_adapter() {
        setup_plan_test_env();

        {
            let mut p = Planner::new();
            p.push(PipelineComponent {
                xform: Transformer(vec![Value::str("command-1"),
                                        Value::from(Identifier::new("foo"))]),
                link: Some(PipeMode::Pipe)
            }).expect("Failed to push pipeline component 1");
            p.push(PipelineComponent {
                xform: Transformer(vec![Value::from(Identifier::new("test-symbol"))]),
                link: None
            }).expect("Failed to push pipeline component 2");

            // verify that it compiles properly
            let r = p.compile().expect("Failed to compile plan");
            assert_eq!(r, Plan {
                steps: vec![PlanStep::FromStdin,
                    PlanStep::Command {
                        exec: PathBuf::from("/test/command-1"),
                        invoked_name: String::from("command-1"),
                        args: vec![b"foo"[..].to_owned()]
                    },
                    PlanStep::EvalGroup {
                        in_type: EvalInputType::PolyObject,
                        out_type: EvalOutputType::PrettyPrint,
                        body: vec![global().get("test-symbol").unwrap()]
                    },
                    PlanStep::ToStdout]
            });
        }
    }
}
