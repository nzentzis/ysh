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
        let (o,i) = unistd::pipe2(fcntl::O_CLOEXEC)
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

        let mut job = Job::new();
        let mut steps_iter = plan.steps().into_iter().peekable();
        while let Some(step) = steps_iter.next() {
            match step {
                PlanStep::FromStdin => {
                    last_output = Some(FDWrapper::Stdin)
                },
                PlanStep::FromFile(path) => {
                    let f = fs::File::open(path).map_err(LaunchError::IO)?;
                    last_output = Some(FDWrapper::Pipe(f.into_raw_fd()));
                },
                p @ PlanStep::EvalGroup {..} => {
                    // if the next one is stdout, we don't need to create a pipe
                    let out_chan = match steps_iter.peek() {
                        Some(PlanStep::ToStdout) => Some(FDWrapper::Stdout),
                        Some(PlanStep::ToFile {..}) => unimplemented!(),
                        Some(PlanStep::ToVar {..}) => unimplemented!(),
                        _ => None
                    };

                    if let Some(f) = out_chan {
                        TransformEvaluation::launch(&mut job,
                                                    last_output.take(), f, p)?;
                        last_output = None;
                    } else {
                        let (i,o) = FDWrapper::pipe_pair();
                        TransformEvaluation::launch(&mut job,
                                                    last_output.take(), i, p)?;
                        last_output = Some(o);
                    }
                },
                PlanStep::Command {exec, invoked_name, args} => {
                    // check whether to link the output to stdout
                    let stdout_channel = match steps_iter.peek() {
                        Some(PlanStep::ToStdout) => Some(IoChannel::Inherited),
                        Some(PlanStep::ToFile {..}) => {
                            unimplemented!()
                        },
                        Some(PlanStep::ToVar {..}) => {
                            unimplemented!()
                        }
                        _ => None
                    };

                    // save the output FD so we can reacquire it in the event of
                    // a failure, since we'd leak the FD otherwise
                    let out_fd = last_output.as_ref().unwrap().as_fd();

                    // set up the command
                    // TODO: redirect or handle stderr
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
                        .stdout(stdout_channel.unwrap_or(IoChannel::Pipe));
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

                    if stdout_channel.is_none() {
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
            let res = match res {
                Ok(r) => r,
                Err(e) => {
                    writeln!(io::stderr(),
                    "ysh: {:?} - {}", expr, e).unwrap();
                    return;
                }
            };

            let to_write = match out_type {
                EvalOutputType::IntoStr => {
                    match res.into_str().wait() {
                        Ok(r) => r,
                        Err(e) => {
                            writeln!(io::stderr(),
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
                            writeln!(io::stderr(),
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
                    writeln!(io::stdout(), "{}", to_write).unwrap();
                },
                FDWrapper::Pipe(fd) => {
                    let mut f = unsafe {
                        ::std::fs::File::from_raw_fd(fd)
                    };
                    writeln!(f, "{}", to_write).unwrap();

                    // don't close the file here so the FDWrapper gets a chance
                    f.into_raw_fd();
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
