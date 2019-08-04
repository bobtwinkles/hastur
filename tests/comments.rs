#[macro_use]
extern crate log;

#[macro_use]
mod util_macros;

use hastur::{NameCache, Engine};
use std::fs::File;

#[test]
fn simple_prefix() {
    env_logger::builder().is_test(true).init();

    {
        let mut cwd = std::env::current_dir().unwrap();
        cwd.push("tests");
        assert!(std::env::set_current_dir(&cwd).is_ok());
    }

    let mut engine = Engine::default();
    let mut names = NameCache::default();

    let inf = File::open("prefix.mk").expect("Failed to open prefix.mk");
    let mut bufreader = std::io::BufReader::new(inf);

    engine.read_makefile(&mut names, &mut bufreader, "prefix.mk").unwrap();

    info!("Database state {:?}", engine.database);

    variable_set_to!(names, engine, "SHELL", "/bin/sh");
    has_target_with_deps!(names, engine, ".DELETE_ON_ERROR", []);
    has_target_with_deps!(names, engine, ".SUFFIXES", [".hpux_make_needs_suffix_list"]);
    has_target_with_deps!(names, engine, ".SILENT", []);
    has_target_with_deps!(names, engine, "cmake_force", []);
    has_target_with_deps!(names, engine, ".PHONY", ["cmake_force"]);
}
