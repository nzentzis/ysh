pub mod core;

mod operators;
mod sequence;
mod async;
mod filesystem;

pub fn initialize() {
    core::initialize();

    operators::initialize();
    sequence::initialize();
    async::initialize();
    filesystem::initialize();
}
