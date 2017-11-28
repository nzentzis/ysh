mod operators;
mod sequence;
mod core;

pub fn initialize() {
    operators::initialize();
    sequence::initialize();
    core::initialize();
}
