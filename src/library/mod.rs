pub mod core;

mod operators;
mod sequence;
mod async;
mod filesystem;
mod builtins;
mod io;

pub fn initialize() {
    core::initialize();

    builtins::initialize();
    operators::initialize();
    sequence::initialize();
    async::initialize();
    filesystem::initialize();
    io::initialize();
}
