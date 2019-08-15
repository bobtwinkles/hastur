#[macro_use]
extern crate log;

use hastur::{NameCache, Engine};
use std::fs::File;

fn main() {
    env_logger::builder().init();

    let mut names = NameCache::default();

    let mut args = std::env::args();
    // First argument is the binary itself, don't process that
    args.next();

    for arg in args {
        let mut engine = Engine::default();

        let inf = File::open(&arg).expect(&format!("Failed to open file {:?}", arg));
        let mut bufreader = std::io::BufReader::new(inf);

        engine.read_makefile(&mut names, &mut bufreader, &arg).expect(&format!("Failed to parse {:?}", arg));

        for rule in engine.database.rules() {
            let target_name = names.resolve_file_name(rule.target_fname()).unwrap();
            info!("{}", target_name);
            let mut deps = String::new();
            let mut fnl = "";
            for dep in rule.dependencies() {
                deps.push_str(fnl);
                deps.push_str(&dep.into_string());

                fnl = " ";
            }

            info!(" - {}", deps);
        }
    }
}
