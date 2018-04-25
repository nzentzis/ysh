pub mod core;

mod operators;
mod sequence;
mod async;
mod filesystem;
mod builtins;
mod io;

fn run_string(s: &'static str) {
    use std::io::Cursor;
    use data::ValueLike;
    use stream::ReadWrapper;
    use reader::ParseError;

    let mut cursor = Cursor::new(s);
    let f = ReadWrapper::new(&mut cursor);
    let mut env = ::environment::empty();
    loop {
        let m = match ::reader::read(&f).result {
            Ok(r) => r,
            Err(e) => if let ParseError::UnexpectedEOF = e {break;}
                      else {panic!("Failed to parse Lisp core library")}
        };
        m.evaluate(&mut env).wait()
         .expect("Failed to evaluated Lisp core library form");
    }
}

pub fn initialize() {
    core::initialize();

    builtins::initialize();
    operators::initialize();
    sequence::initialize();
    async::initialize();
    filesystem::initialize();
    io::initialize();

    // initialize core Lisp library
    let lib_sources = include_str!("init.ysh");
    run_string(lib_sources);
}
