# ysh - Fast, functional, and ergonomic command-line environment
`ysh` is an alternative shell which aims to integrate the power and
expressiveness of Lisp with the easy access to external software and powerful
stream abstractions of POSIX shells. It focuses on performance, efficiency,
high-level expressive power, and familiarity to those used to POSIX-like shells.

# Pending Tasks
    [ ] Core language support
    -[ ] Reader macros
    -[X] Hashmap type
    -[ ] Arbitrary-precision arithmetic support in numeric tower
    -[ ] Guaranteed tail-call optimization
    -[X] Atoms
    -[ ] Symbol/atom interning
    -[ ] Exceptions and handling
    -[ ] Use persistent data structures in place of native Vec
    -[X] Wrapper interface for global value-like objects (e.g. `path` and such)
    -[ ] Continuations
    -[ ] Defined binary serialization format w/ fast native-code encoder
    [ ] Standard library
    -[ ] Basic functions
    --[ ] Lists
    --[ ] Numerics
    --[ ] Strings and formatting
    --[ ] Control flow and conditionals
    -[ ] Streams and I/O (`io/*`)
    --[ ] Colorized terminal output
    --[ ] Termcap interface
    --[ ] Global stream abstraction
    -[ ] Environment
    --[X] Environment variables
    ---[ ] Integrate with reader
    -[ ] Async (`async/*`)
    --[ ] Deferred/future values
    --[ ] Asynchronous tasks (integrate with job control system)
    --[ ] Parallel `map`/`reduce`/`filter` functions
    -[ ] Filesystem (`fs/*`)
    --[ ] Stat queries
    --[ ] Manipulation
    --[ ] Globbing
    [ ] Built-in commands
    -[ ] Directory management (`cd`, `pwd`, `pushd`, `popd`, etc)
    -[ ] Job management (`fg`, `bg`, `jobs`, `kill`)
    [ ] Support `.yshrc` files
    [ ] Pipeline adapters and endpoints
    -[ ] Terminal elements
    -[ ] Source elements
    -[ ] Stream parsing support
    -[ ] Pretty-printing output
    -[X] Automatic symbol globbing in command transformers
    [ ] Job control
    -[ ] Backgrounding jobs (via `&` or some other means)
    -[ ] Job control interface functions
    -[ ] Reattaching backgrounded output streams
    --[ ] Use ptrace to hijack launched command's I/O streams?
    -[ ] Integrate with futures/async tasks
    [ ] Line editing
    -[ ] Vim bindings
    -[ ] Readline bindings
    -[ ] Expose line editor to user code
    -[ ] Multi-line input based on reader results
    --[ ] Better reader output type
    -[ ] Extensible binding support
    -[ ] Allow implementing editing disciplines in user code
    -[ ] Completions
    --[ ] Expose to user code
    --[ ] Allow completion overrides from user code
    [ ] History support
    -[X] Store commands in history
    -[X] Expose to user code
    -[ ] Built-in support for recall and search
    -[ ] Semantic history (store/expose as values, search by filter)
    -[ ] Store previous command results
    [ ] Debugging & Profiling
    -[ ] User code debugger
    -[ ] Memory profiling and optimization
    -[ ] CPU usage profiling
    [ ] Documentation
    -[ ] HTML/PDF manual and tutorial
    -[ ] Standard library reference
    -[ ] Smart `man` command which can pull doc data out of the passed object
    [ ] Quality-of-life Improvements
    -[ ] Destructuring in `let`/`fn`
    -[ ] `#()` shorthand for embedded pipelines (swap with current `$()` lambda?)
    -[ ] Support lenses for easier filtering over streams

# Ideas for Future Exploration

* Multiple REPLs (shells) referencing single shared backend
* Transparent remote commands - share environment and data over SSH connection
* Automatically decode header-prefixed columnar output into list-of-maps
* Allow shells inside `sudo` to close over current environment
