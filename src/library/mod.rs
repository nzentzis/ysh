pub mod core;

mod operators;
mod sequence;
mod async;

pub fn initialize() {
    core::initialize();

    operators::initialize();
    sequence::initialize();
    async::initialize();
}
