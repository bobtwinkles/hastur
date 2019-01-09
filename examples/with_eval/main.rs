use hastur::Database;

fn main() {
    let database = Database::from_file("test.mk");
    let rule = database.get_rule_for_target("default");

    println!("Building the target `default` requires the following objects to be built:");
    // `rule.dependencies` shall produce an iterator outputting hastur::trees::Evaluated objects,
    // one for each dependency.
    for dep in rule.dependencies() {
        println!(" - {}, {:?}", dep.into_string(), dep);
    }
}
