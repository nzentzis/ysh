pub mod core;

mod operators;
mod sequence;
mod async;
mod filesystem;
mod builtins;

pub fn initialize() {
    core::initialize();

    builtins::initialize();
    operators::initialize();
    sequence::initialize();
    async::initialize();
    filesystem::initialize();
}
