use std::io;
use std::path::PathBuf;
use std::os::unix::prelude::*;
use std::error::Error;
use std::fs;

use nix::fcntl;
use nix::unistd;

use data::*;
use environment::{global, empty, run_fn};
use jobs::{Command, Job, IoChannel};
use stream::*;
use planner::{Plan, PlanStep, EvalInputType, EvalOutputType};

/// Errors which can arise from attempting to launch a pipeline
#[derive(Debug)]
pub enum LaunchError {
    Evaluation(EvalError),
    JobLaunch(::nix::Error),
    IO(io::Error),
    Unknown
}

#[derive(PartialEq, Eq)]
enum FDWrapper {
    Stdin, Stdout,
    Pipe(RawFd)
}

// TODO: adapt this to use the terminal type
impl FDWrapper {
    fn as_fd(&self) -> RawFd {
        match self {
            &FDWrapper::Stdin   => io::stdin().as_raw_fd(),
            &FDWrapper::Stdout  => io::stdout().as_raw_fd(),
            &FDWrapper::Pipe(f) => f,
        }
    }

    fn into_fd(self) -> RawFd {
        let r = match self {
            FDWrapper::Stdin   => io::stdin().as_raw_fd(),
            FDWrapper::Stdout  => io::stdout().as_raw_fd(),
            FDWrapper::Pipe(f) => f,
        };

        // avoid dropping self so we don't close FDs
        ::std::mem::forget(self);

        r
    }

    fn pipe_pair() -> (FDWrapper, FDWrapper) {
        let (i,o) = unistd::pipe2(fcntl::O_CLOEXEC)
                           .expect("cannot generate pipes");
        let i = FDWrapper::Pipe(i);
        let o = FDWrapper::Pipe(o);
        (i,o)
    }
}

impl Drop for FDWrapper {
    fn drop(&mut self) {
        match self {
            &mut FDWrapper::Pipe(f) => {
                unistd::close(f).expect("failed to close wrapped FD");
            },
            _ => {}
        }
    }
}

impl ActivePipeline {
    /// Launch a pipeline from a provided plan
    pub fn launch(plan: &Plan,
                  background: bool) -> Result<ActivePipeline, LaunchError> {
        // fd of the last output
        let mut last_output = None;

        let last_elem = plan.steps().last().unwrap();

        let mut job = Job::new();
        for step in plan.steps() {
            match step {
                PlanStep::FromStdin => {
                    last_output = Some(FDWrapper::Stdin)
                },
                PlanStep::FromFile(path) => {
                    let f = fs::File::open(path).map_err(LaunchError::IO)?;
                    last_output = Some(FDWrapper::Pipe(f.into_raw_fd()));
                },
                p @ PlanStep::EvalGroup {..} => {
                    let (i,o) = FDWrapper::pipe_pair();
                    TransformEvaluation::launch(&mut job,
                                                last_output.take(), i, p)?;
                    last_output = Some(o);
                },
                PlanStep::Command {exec, invoked_name, args} => {
                    // check whether to link the output to stdout
                    let fwd_stdout = match last_elem {
                        PlanStep::ToStdout => true,
                        _                  => false
                    };
                    // let fwd_stdout = elems_iter.peek() == Some(&&PlanElement::Stdout);

                    // set up the command
                    // TODO: redirect or handle stderr
                    let out_fd = last_output.as_ref().unwrap().as_fd();
                    let stdin = if last_output == Some(FDWrapper::Stdin) {
                        IoChannel::Inherited
                    } else {
                        IoChannel::Specific(last_output.take().unwrap().into_fd())
                    };
                    let cmd = Command::new(exec)
                        .invoked_using(invoked_name)
                        .args(args.into_iter()
                                  .map(|x| ::std::ffi::OsStr::from_bytes(x))
                                  .collect::<Vec<_>>())
                        .stdin(stdin)
                        .stdout(if fwd_stdout {IoChannel::Inherited}
                                else {IoChannel::Pipe});
                    let cmd = if background { cmd } else { cmd.foreground() };
                    
                    // launch it into the job
                    let process = match job.launch(cmd) {
                        Ok(p) => p,
                        Err(e) => {
                            // retake output FD to avoid leaking it
                            FDWrapper::Pipe(out_fd);
                            return Err(LaunchError::JobLaunch(e));
                        }
                    };

                    if !fwd_stdout {
                        last_output = Some(FDWrapper::Pipe(
                                process.stdout().unwrap().into_raw_fd()));
                    }
                },
                PlanStep::ToStdout => {},
                PlanStep::ToVar {..} => {},
                PlanStep::ToFile {..} => {},
            }
        }

        Ok(ActivePipeline { job })

        /*
        // execute all plan elements
        let mut job = Job::new();
        let mut plan_buffer = Vec::with_capacity(self.elems.len());
        while let Some(element) = elems_iter.next() {
            match element {
                &PlanElement::Expression(ref v) =>
                    plan_buffer.push(PlanElement::Expression(v.to_owned())),
                &PlanElement::Adapter(ref t) =>
                    plan_buffer.push(PlanElement::Adapter(*t)),
                &PlanElement::Command {ref exec, ref invoked_name, ref args} => {
                    // we have to flush the plan buffer if there's anything
                    // there
                    if !plan_buffer.is_empty() {
                        // generate a pipe
                        let (i,o) = unistd::pipe2(fcntl::O_CLOEXEC)
                                           .expect("cannot generate pipes");
                        let i = FDWrapper::Pipe(i);
                        let o = FDWrapper::Pipe(o);
                        let eval = TransformEvaluation::launch(&mut job,
                                        last_output.take().unwrap(),
                                        plan_buffer.drain(..),
                                        EvalOutput::Descriptor(i));

                        if eval.is_none() {
                            // handle launch failure
                            // currently just close FDs and abort
                            // TODO: specific error code
                            return Err(LaunchError::Unknown);
                        }
                        last_output = Some(o);
                    }

                    // check whether to link the output to stdout
                    let fwd_stdout = elems_iter.peek() == Some(&&PlanElement::Stdout);

                    // evaluate args
                    let args = args.into_iter()
                                   .map(|a| a.evaluate(&empty()))
                                   .collect::<Eval<Vec<_>>>().wait()
                                   .and_then(|a|
                                         a.into_iter()
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
                                              _ => x
                                          })
                                          .map(|x| match &x.data {
                                              &ValueData::Symbol(_) =>
                                                  run_fn("fs/glob", &[x.clone()])
                                                 .map(|x| x.wait())
                                                 .unwrap_or(Ok(x.clone()))
                                                 .map(|l|
                                                      if l.into_seq().wait()
                                                          .unwrap().len() == 0 {
                                                          x.clone()
                                                      } else {
                                                          l
                                                      }),
                                              &ValueData::Str(_) =>
                                                  run_fn("fs/glob", &[x.clone()])
                                                 .map(|x| x.wait())
                                                 .unwrap_or(Ok(x.clone()))
                                                 .map(|l|
                                                      if l.into_seq()
                                                          .wait().unwrap().len() == 0 {
                                                          x.clone()
                                                      } else {
                                                          l
                                                      }),
                                              _ => Ok(x)
                                          })
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
                        Err(e) => {
                            // TODO: kill existing components
                            return Err(LaunchError::Evaluation(e));
                        }
                    };

                    // TODO: redirect or handle stderr
                    let out_fd = last_output.as_ref().unwrap().as_fd();
                    let cmd = Command::new(exec)
                                      .invoked_using(invoked_name)
                                      .args(args)
                                      .stdin(
                                          if last_output == Some(FDWrapper::Stdin) {
                                              IoChannel::Inherited }
                                          else {IoChannel::Specific(last_output
                                                                   .take()
                                                                   .unwrap()
                                                                   .into_fd())})
                                      .stdout(
                                          if fwd_stdout { IoChannel::Inherited }
                                          else { IoChannel::Pipe });
                    let cmd = if background { cmd }
                              else { cmd.foreground() };
                    let process = match job.launch(cmd) {
                        Ok(p) => p,
                        Err(e) => {
                            // retake ownership of output FD
                            FDWrapper::Pipe(out_fd);
                            return Err(LaunchError::JobLaunch(e));
                        }
                    };

                    if !fwd_stdout {
                        last_output = Some(FDWrapper::Pipe(
                                process.stdout()
                                       .unwrap()
                                       .into_raw_fd()));
                    }
                },
                _ => {}
            }
        }

        // if there's a plan in the buffer, we need to flush it
        //
        // NOTE: we don't need to worry about having the output already
        // terminated here. The plan buffer will have items iff the previous
        // element wasn't a command. If it was, the buffer would have been
        // flushed already as part of launching it.
        if !plan_buffer.is_empty() {
            // figure out what output to use
            let out = match self.elems.last().unwrap() {
                &PlanElement::Stdout         => EvalOutput::PrettyStdout,
                &PlanElement::AppendFile(_)  => unimplemented!(),
                &PlanElement::AppendVar(_ )  => unimplemented!(),
                &PlanElement::ToFile(_)      => unimplemented!(),
                &PlanElement::IntoVar(_)     => unimplemented!(),
                _                            => panic!("invalid plan terminator")
            };

            let eval = TransformEvaluation::launch(&mut job,
                last_output.take().unwrap(), plan_buffer.drain(..), out);
            if eval.is_none() {
                // TODO close output FDs here
                // TODO: specific error code
                return Err(LaunchError::Unknown);
            }
        }

        Ok(ActivePipeline { job })
        */
    }
}

enum EvalOutput {
    PrettyStdout,
    Descriptor(FDWrapper)
}

/// Representation of a transformer evaluation which is running as part of a
/// pipeline.
/// 
/// Each transformer runs in its own thread, and is responsible for driving a
/// collection of Lisp functions. The TransformEvaluation is restricted to the
/// remote thread, but it generates a TransformHandle that can be used to manage
/// its execution.
struct TransformEvaluation {
}

impl TransformEvaluation {
    /// Launch a new transformer evaluation based on a passed `EvalGroup` step
    ///
    /// # Panics
    /// If the step is not an `EvalGroup` or is an invalid (read: empty) group,
    /// this function will panic.
    fn launch(job: &mut Job,
              input: Option<FDWrapper>,
              output: FDWrapper,
              step: &PlanStep)
            -> Result<TransformEvaluation, LaunchError> {
        use std::io::prelude::*;
        use terminal;

        let (in_type, out_type, body) =
            if let PlanStep::EvalGroup {in_type,out_type,body} = step {
                (in_type, out_type, body)
            } else {
                panic!("Tried to launch a non-evalgroup plan step");
            };

        let mut elements = body.into_iter();

        // build the innermost (first generated) value from the input
        // since we the pipeline gets built from the inside (left element) out
        // (toward the right) we need to keep track of the current element at
        // all times.
        let mut innermost = {
            // building the innermost element just means wrapping the stream in
            // an appropriate adapter
            //
            // build the adapter
            let opts = match in_type {
                EvalInputType::NoInput => None,
                EvalInputType::PolyObject => Some(StreamOptions::new()),
                EvalInputType::StreamDelim(c) => {
                    let mut o = StreamOptions::new();
                    o.delimiter(*c);
                    Some(o)
                },
                EvalInputType::StreamString => Some(StreamOptions::basic())
            };

            // wrap the input stream in it
            if let Some(input) = input {
                if let Some(opts) = opts {
                    Value::new(if input == FDWrapper::Stdin {
                        PolyStream::from_stdin(opts)
                                   .expect("Cannot construct input stream")
                    } else {
                        PolyStream::from_fd(input.into_fd(), opts)
                                   .expect("Cannot construct input stream")
                    })
                } else {
                    elements.next().expect("Tried to launch an invalid step")
                            .to_owned()
                }
            } else {
                if let Some(opts) = opts {
                    panic!("No input supplied to EvalGroup which requires it")
                } else {
                    elements.next().expect("Tried to launch an invalid step")
                            .to_owned()
                }
            }
        };

        // TODO: support killing active transforms
        for elem in elements {
            // turn it into a transform, and update the current innermost form
            innermost = TransformEvaluation::apply_value_xform(elem.to_owned(),
                                                               innermost);
        }

        // macroexpand form before evaluating it
        let expr = innermost.macroexpand().wait()
                  .map_err(LaunchError::Evaluation)?;

        // launch the transform
        let out = output;
        let out_type = out_type.to_owned();
        job.spawn(move || {
            // TODO: error handling
            let res = expr.evaluate(&::environment::empty()).wait();
            let mut term = terminal::acquire();
            let res = match res {
                Ok(r) => r,
                Err(e) => {
                    writeln!(term.stderr(), "ysh: {:?} - {}", expr, e).unwrap();
                    return;
                }
            };
            let _ = term;

            let to_write = match out_type {
                EvalOutputType::IntoStr => {
                    match res.into_str().wait() {
                        Ok(r) => r,
                        Err(e) => {
                            let mut term = terminal::acquire();
                            writeln!(term.stderr(),
                                     "ysh: cannot convert to string: {}", e)
                                .unwrap();
                            return;
                        }
                    }
                },
                EvalOutputType::PrettyPrint => {
                    match res.into_str().wait() {
                        Ok(r) => r,
                        Err(e) => {
                            let mut term = terminal::acquire();
                            writeln!(term.stderr(),
                                     "ysh: cannot convert to string: {}", e)
                                .unwrap();
                            return;
                        }
                    }
                },
                EvalOutputType::Object => unimplemented!(),
            };

            match out {
                FDWrapper::Stdout => {
                    let mut t = terminal::acquire();
                    writeln!(t.stdout(), "{}", to_write).unwrap();
                },
                FDWrapper::Pipe(fd) => {
                    let mut f = unsafe {
                        ::std::fs::File::from_raw_fd(fd)
                    };
                    writeln!(f, "{}", to_write).unwrap();
                },
                FDWrapper::Stdin => panic!("Cannot output to stdin")
            }
        });

        Ok(TransformEvaluation {})
    }

    /// Generate a transformed expression from a basic one using the following
    /// rules:
    /// 
    /// 1. If the transform value is executable:
    ///     a. Convert it to a sequence (if this fails, return the value)
    ///     b. Append the inner value
    ///     c. Return the result
    /// 2. Evaluate the transform. If the result is executable, perform steps
    ///    a, b, and c on the result.
    /// 3. If the evaluation succeeded, return a quoted evaluation result and
    ///    ignore the input
    /// 4. If the evaluation failed, use the original value
    fn apply_value_xform(xform: Value, inner: Value) -> Value {
        use std::ops::Deref;
        if xform.is_executable() {
            return Value::list(vec![xform, inner])
        } else if let Ok(Some(s)) = xform.get_symbol().wait() {
            // try looking up the symbol to get an executable result
            let val = global().get(s.0.deref());
            match val {
                Some(ref x) if x.is_executable() => {
                    let mut arr = vec![x.deref().to_owned()];
                    let args = if let Ok(r) = xform.into_seq().wait() {r}
                               else {return xform;};
                    arr.extend(args.into_iter().skip(1));
                    arr.push(inner);
                    return Value::list(arr);
                },
                _ => {}
            }
        }

        let modified_xform = xform.clone().macroexpand().wait()
                                  .and_then(|e| e.evaluate(&::environment::empty())
                                                 .wait());

        if let Ok(m) = modified_xform {
            if m.is_executable() {
                return Value::list(vec![m.clone(), inner]);
            } else {
                return ::library::core::quote(&[m])
            }
        } else {
            xform
        }
    }
}

/// Representation of an active pipeline.
pub struct ActivePipeline {
    /// OS processes
    job: Job,
}

impl ActivePipeline {
    /// Wait until all the processes in the pipeline have terminated
    pub fn wait(mut self) {
        self.job.wait().expect("wait failed");
    }
}
