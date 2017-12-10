use std::os::unix::prelude::*;
use std::cell::RefCell;
use std::ffi::CString;
use std::ffi;
use std::thread;

use nix;
use nix::sys::wait;
use nix::sys::signal;
use nix::fcntl;
use nix::unistd;

use libc;

/// Read end of a Unix pipe
pub struct PipeReader(RawFd);

/// Write end of a Unix pipe
pub struct PipeWriter(RawFd);

pub fn pipe() -> nix::Result<(PipeReader, PipeWriter)> {
    let (r,w) = unistd::pipe2(fcntl::O_CLOEXEC)?;
    Ok((PipeReader(r), PipeWriter(w)))
}

impl IntoRawFd for PipeReader {
    fn into_raw_fd(self) -> RawFd { self.0 }
}

impl IntoRawFd for PipeWriter {
    fn into_raw_fd(self) -> RawFd { self.0 }
}

impl Drop for PipeReader {
    fn drop(&mut self) {
        unistd::close(self.0).unwrap_or(());
    }
}

impl Drop for PipeWriter {
    fn drop(&mut self) {
        unistd::close(self.0).unwrap_or(());
    }
}

/// An I/O channel to pass to the child process
/// 
/// Keep in mind that the file descriptor passed to a raw IO channel *will* be
/// closed.
#[derive(Copy, Clone, PartialEq)]
pub enum IoChannel {
    Inherited,
    Pipe,
    Specific(RawFd)
}

impl IoChannel {
    /// Returns the child FD and parent FD (if any)
    fn prepare_fds(&self, is_input: bool, inherit: RawFd)
            -> nix::Result<(RawFd, Option<RawFd>)> {
        Ok(match self {
            &IoChannel::Inherited => {
                // clone the FD
                let new_fd = fcntl::fcntl(
                                inherit,
                                fcntl::FcntlArg::F_DUPFD_CLOEXEC(inherit))?;
                (new_fd, None)
            },
            &IoChannel::Pipe => {
                let (r,w) = unistd::pipe2(fcntl::O_CLOEXEC)?;
                if is_input { (r, Some(w)) }
                else        { (w, Some(r)) }
            },
            &IoChannel::Specific(fd) => {
                (fd, None)
            }
        })
    }
}

pub struct Command {
    executable: CString,
    apparent_exe: CString,
    argv: Vec<CString>,
    stdin: IoChannel,
    stdout: IoChannel,
    stderr: IoChannel,
    foreground: bool
}

impl Command {
    /// Generate a new command builder
    pub fn new<P: AsRef<ffi::OsStr>>(exe: P) -> Self {
        Command {
            executable: CString::new(exe.as_ref().to_os_string().into_vec()).unwrap(),
            apparent_exe: CString::new(exe.as_ref().to_os_string().into_vec()).unwrap(),
            argv: vec![],
            stdin: IoChannel::Inherited,
            stdout: IoChannel::Inherited,
            stderr: IoChannel::Inherited,
            foreground: false
        }
    }

    /// Set the command to run in the foreground
    pub fn foreground(mut self) -> Self {
        self.foreground = true;
        self
    }

    /// Set the apparent executable name
    pub fn invoked_using<P: AsRef<ffi::OsStr>>(mut self, name: P) -> Self {
        self.apparent_exe =
            CString::new(name.as_ref().to_os_string().into_vec()).unwrap();
        self
    }

    /// Set the argument list
    pub fn args<S,I>(mut self, args: I) -> Self
            where I: IntoIterator<Item=S>,
                  S: AsRef<ffi::OsStr> {
        for i in args.into_iter() {
            self.argv.push(CString::new(i.as_ref()
                                         .to_os_string()
                                         .into_vec())
                                   .unwrap());
        }
        self
    }

    /// Set the stdin mode
    pub fn stdin(mut self, input: IoChannel) -> Self {
        self.stdin = input;
        self
    }

    /// Set the stdout mode
    pub fn stdout(mut self, output: IoChannel) -> Self {
        self.stdout = output;
        self
    }

    /// Set the stderr mode
    pub fn stderr(mut self, err: IoChannel) -> Self {
        self.stderr = err;
        self
    }

    /// Launch the process
    /// 
    /// If the `group` option is `Some`, then the process will be moved into a
    /// new process group. The new PGID will be equal to its PID if the content
    /// of `group` is negative, and the given value otherwise.
    fn launch_in_pgrp(self, group: Option<Option<unistd::Pid>>)
            -> nix::Result<Process> {
        // get FDs for in/out/err
        let (in_chld, in_par) = self.stdin.prepare_fds(true, 0)?;
        let (out_chld, out_par) = self.stdout.prepare_fds(false, 1)?;
        let (err_chld, err_par) = self.stderr.prepare_fds(false, 2)?;

        // prep arguments
        let mut args = vec![self.apparent_exe.clone()];
        args.extend(self.argv.iter().cloned());

        // fork
        match unistd::fork()? {
            unistd::ForkResult::Parent { child, .. } => {
                // clean up file descriptors
                unistd::close(in_chld)?;
                unistd::close(out_chld)?;
                unistd::close(err_chld)?;

                Ok(Process {
                    pid: child,
                    pgid: unistd::getpgid(Some(child)).unwrap(),
                    input: RefCell::new(in_par),
                    output: RefCell::new(out_par),
                    error: RefCell::new(err_par),
                    state: ProcessStatus::Running
                })
            },
            unistd::ForkResult::Child => {
                if let Some(f) = in_par { unistd::close(f).unwrap() }
                if let Some(f) = out_par { unistd::close(f).unwrap() }
                if let Some(f) = err_par { unistd::close(f).unwrap(); }
                let _ = self.post_fork((in_chld, out_chld, err_chld),
                                       group, args);

                // if we reach this point, we failed to exec
                ::std::process::exit(1)
            }
        }
    }

    /// Function to run after forking
    /// 
    /// This operates in a constrained environment - we can't assume that any
    /// other threads or globals are in a reasonable state. We *can* access
    /// atomic variables, though.
    fn post_fork(self, ioe: (RawFd, RawFd, RawFd),
                 group: Option<Option<unistd::Pid>>, args: Vec<CString>)
                -> nix::Result<()> {
        // adjust PGID and controlling terminal
        let me = unistd::Pid::this();
        if let Some(p) = group {
            let pgid = if let Some(pg) = p {
                unistd::setpgid(me, pg)?;
                pg
            } else {
                unistd::setpgid(me, me)?;
                me
            };

            if self.foreground {
                unistd::tcsetpgrp(0, pgid)?;
            }
        }

        // reset our signals
        unsafe {
            let default = signal::SigAction::new(signal::SigHandler::SigDfl,
                                                 signal::SA_RESTART,
                                                 signal::SigSet::empty());
            signal::sigaction(signal::SIGINT, &default)?;
            signal::sigaction(signal::SIGQUIT, &default)?;
            signal::sigaction(signal::SIGTSTP, &default)?;
            signal::sigaction(signal::SIGTTIN, &default)?;
            signal::sigaction(signal::SIGTTOU, &default)?;
            signal::sigaction(signal::SIGCHLD, &default)?;
        }

        // move FDs around
        unistd::dup2(ioe.0, 0).unwrap(); // stdin
        unistd::dup2(ioe.1, 1).unwrap(); // stdout
        unistd::dup2(ioe.2, 2).unwrap(); // stderr
        if ioe.0 != 0 { unistd::close(ioe.0).unwrap(); }
        if ioe.1 != 1 { unistd::close(ioe.1).unwrap(); }
        if ioe.2 != 2 { unistd::close(ioe.2).unwrap(); }

        // execute!
        unistd::execv(&self.executable, args.as_slice()).map(|_| ())
    }
}

pub struct OutChannel(RawFd);

impl IntoRawFd for OutChannel {
    fn into_raw_fd(self) -> RawFd { self.0 }
}

pub struct InChannel(RawFd);

impl IntoRawFd for InChannel {
    fn into_raw_fd(self) -> RawFd { self.0 }
}

pub enum ProcessStatus {
    Exited(i8),
    Stopped,
    Running
}

/// Control handle for an OS-level process
pub struct Process {
    pid: unistd::Pid,
    pgid: unistd::Pid,
    input: RefCell<Option<RawFd>>,
    output: RefCell<Option<RawFd>>,
    error: RefCell<Option<RawFd>>,
    state: ProcessStatus
}

impl Process {
    pub fn stdout(&self) -> Option<OutChannel> {
        self.output.borrow_mut().take().map(OutChannel)
    }

    pub fn stderr(&self) -> Option<OutChannel> {
        self.error.borrow_mut().take().map(OutChannel)
    }

    pub fn stdin(&self) -> Option<InChannel> {
        self.input.borrow_mut().take().map(InChannel)
    }

    /// Wait for the process to terminate or stop
    pub fn wait(&mut self) -> nix::Result<()> {
        loop {
            match wait::waitpid(self.pid, None)? {
                wait::WaitStatus::Exited(_, status) => {
                    self.state = ProcessStatus::Exited(status);
                    break;
                },
                wait::WaitStatus::Signaled(_, _, _) => {
                    self.state = ProcessStatus::Exited(127);
                    break;
                },
                wait::WaitStatus::Stopped(_, _) => {
                    self.state = ProcessStatus::Stopped;
                    break;
                },
                wait::WaitStatus::Continued(_) => {
                    self.state = ProcessStatus::Running;
                },
                _ => {},
            }
        }
        Ok(())
    }

    /// Suspend the process using `SIGSTOP`
    pub fn suspend(&mut self) -> nix::Result<()> {
        signal::kill(self.pid, signal::Signal::SIGSTOP)
    }

    /// Resume the process using `SIGCONT`
    pub fn resume(&mut self) -> nix::Result<()> {
        signal::kill(self.pid, signal::Signal::SIGCONT)
    }
}

/// An active function evaluation which can be controlled as part of a job
pub struct ShellTask {
    handle: Option<thread::JoinHandle<()>>
}

impl ShellTask {
    /// Start a new shell task using the given function
    pub fn start<F: Fn() -> ()+Sync+Send+'static>(f: F) -> Self {
        let h = thread::spawn(move || f());

        ShellTask {
            handle: Some(h)
        }
    }

    /// Suspend the evaluation thread
    pub fn suspend(&mut self) -> nix::Result<()> {
        unimplemented!()
    }

    /// Resume the evaluation thread
    pub fn resume(&mut self) -> nix::Result<()> {
        unimplemented!()
    }

    /// Wait until the evaluation completes
    pub fn wait(&mut self) -> nix::Result<()> {
        if let Some(h) = self.handle.take() {
            h.join().unwrap();
        }
        Ok(())
    }
}

/// A concurrent operation which can be controlled as part of a job.
/// 
/// This can either be an OS-level process or a shell-internal task.
pub enum Task {
    OS(Process),
    Internal(ShellTask)
}

impl Task {
    /// Suspend the task
    pub fn suspend(&mut self) -> nix::Result<()> {
        match self {
            &mut Task::OS(ref mut p) => p.suspend(),
            &mut Task::Internal(ref mut e) => e.suspend(),
        }
    }

    /// Resume the task
    pub fn resume(&mut self) -> nix::Result<()> {
        match self {
            &mut Task::OS(ref mut p) => p.resume(),
            &mut Task::Internal(ref mut e) => e.resume(),
        }
    }

    /// Wait for the task to terminate or change status
    pub fn wait(&mut self) -> nix::Result<()> {
        match self {
            &mut Task::OS(ref mut p) => p.wait(),
            &mut Task::Internal(ref mut e) => e.wait(),
        }
    }
}

/// A structure representing a collection of tasks which can be manipulated as
/// a unit
pub struct Job {
    pgroup: Option<unistd::Pid>,
    tasks: Vec<Task>
}

impl Job {
    pub fn new() -> Self {
        Job {
            pgroup: None,
            tasks: Vec::new()
        }
    }

    /// Launch an additional command into the job
    pub fn launch(&mut self, cmd: Command) -> nix::Result<&Process> {
        let p = cmd.launch_in_pgrp(Some(self.pgroup.clone()))?;
        if self.pgroup.is_none() {
            self.pgroup = Some(p.pgid);
        }

        self.tasks.push(Task::OS(p));
        if let &Task::OS(ref p) = self.tasks.last().unwrap() { Ok(p) }
        else { panic!() }
    }

    /// Start evaluating an internal task as part of this job
    pub fn spawn<F: Fn() -> ()+Sync+Send+'static>(&mut self, f: F) -> &ShellTask {
        let t = ShellTask::start(f);

        self.tasks.push(Task::Internal(t));
        if let &Task::Internal(ref p) = self.tasks.last().unwrap() { p }
        else { panic!() }
    }

    /// Send a signal to all the processes in the group
    fn signal(&mut self, sig: signal::Signal) -> nix::Result<()> {
        let pgrp = if let &Some(ref p) = &self.pgroup { p }
                   else { return Ok(()); };
        let pgrp: unistd::Pid = pgrp.to_owned();

        // convert process group to pid_t
        // TODO: once killpg is available in Nix, use that instead!
        //       this is a dirty unsafe hack!
        let pg_pid: libc::pid_t = unsafe { ::std::mem::transmute(pgrp) };
        let pg_pid = -pg_pid;
        let pg_pid = unistd::Pid::from_raw(pg_pid);

        signal::kill(pg_pid, sig)
    }

    /// Suspend all processes in the group
    pub fn suspend(&mut self) -> nix::Result<()> {
        self.signal(signal::Signal::SIGSTOP)
    }

    /// Resume all processes in the group
    pub fn resume(&mut self) -> nix::Result<()> {
        self.signal(signal::Signal::SIGCONT)
    }

    /// Terminate all processes in the group
    pub fn terminate(&mut self) -> nix::Result<()> {
        self.signal(signal::Signal::SIGTERM)
    }

    /// Forcibly kill all processes in the group
    pub fn kill(&mut self) -> nix::Result<()> {
        self.signal(signal::Signal::SIGKILL)
    }

    /// Wait for all child processes to terminate
    pub fn wait(&mut self) -> nix::Result<()> {
        for c in self.tasks.iter_mut() {
            c.wait()?;
        }

        // retake control of terminal
        unistd::tcsetpgrp(0, unistd::getpgrp())?;
        Ok(())
    }
}
