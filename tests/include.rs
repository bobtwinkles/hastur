#[macro_use]
extern crate log;

#[macro_use]
mod util_macros;

use hastur::{NameCache, Engine};
use std::fs::File;

#[test]
fn simple_includes() {
    env_logger::builder().is_test(true).init();
    let cwd = std::env::current_dir().unwrap();

    let mut engine = Engine::default();
    let mut names = NameCache::default();

    engine.working_directory.push("tests");
    engine.working_directory.push("include");

    let inf = File::open("tests/include/include0.mk").expect("Failed to open test file tests/include/include0.mk");
    let mut bufreader = std::io::BufReader::new(inf);

    engine.read_makefile(&mut names, &mut bufreader, "include0.mk");

    variable_set_to!(names, engine, "foo", "baz");
}
