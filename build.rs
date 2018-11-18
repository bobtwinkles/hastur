extern crate bindgen;
extern crate cc;

use std::env;
use std::path::PathBuf;

fn build_code() {
    let path_base = PathBuf::from("make_src");
    let files = [
        "alloca.c",
        "ar.c",
        "arscan.c",
        "commands.c",
        "default.c",
        "dir.c",
        "expand.c",
        "file.c",
        "function.c",
        "guile.c",
        "hash.c",
        "hastur.c",
        "implicit.c",
        "job.c",
        "load.c",
        "loadapi.c",
        "main.c",
        "misc.c",
        "output.c",
        "posixos.c",
        "read.c",
        "remake.c",
        // "remote-stub.c",
        "rule.c",
        "signame.c",
        "strcache.c",
        "variable.c",
        "version.c",
        "vpath.c",
    ];

    cc::Build::new()
        .files(files.iter().map(|x| path_base.join(x)))
        .include(path_base)
        .define("HASTUR", None)
        .compile("hastur-make");
}

fn generate_bindings() {
    let mut source_path = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    source_path.push("make_src");
    let wrapper_path = source_path.join("bindgen_wrapper.h");
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());

    let bindings = bindgen::Builder::default()
        .header(wrapper_path.to_string_lossy())
        .rustfmt_bindings(true)
        .whitelist_function("read_all_makefiles")
        .whitelist_function("hastur_.*")
        .whitelist_function("variable_expand")
        .whitelist_function("variable_expand_for_file")
        .whitelist_function("allocated_variable_expand_for_file")
        .whitelist_function("gmk_.*")
        .whitelist_var("HASTUR_.*")
        .whitelist_var("RUST_.*")
        .clang_args(["-isystem", &source_path.to_string_lossy()].iter());

    let bindings = bindings
        .generate()
        .expect("Failed to generate make bindings");
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings");
}

fn main() {
    build_code();
    generate_bindings()
}
