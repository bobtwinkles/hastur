#[macro_use]
extern crate log;

use hastur::{Engine, NameCache};
use std::fs::File;

fn main() {
    env_logger::builder().init();

    let mut args: Vec<_> = std::env::args().collect();

    let mut opts = getopts::Options::new();
    opts.optopt("C", "", "Directory to treat as the root", "DIRECTORY");

    let matches = match opts.parse(&args[1..]) {
        Ok(o) => o,
        Err(e) => panic!(e.to_string()),
    };

    let mut names = NameCache::default();

    for arg in matches.free.iter() {
        let mut engine = Engine::default();

        if let Some(root) = matches.opt_str("C") {
            engine.working_directory = root.into();
        }

        let inf = File::open(&arg).expect(&format!("Failed to open file {:?}", arg));
        let mut bufreader = std::io::BufReader::new(inf);

        engine
            .read_makefile(&mut names, &mut bufreader, &arg)
            .expect(&format!("Failed to parse {:?}", arg));

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
