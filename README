HASTUR
======

Hastur is a Rust library for interacting with makefiles. Eventually, you should
be able to extract most pieces of information from any given `Makefile` and
interactively explore the set of variables and definitions that result after
evaluation. Right now, it has a pretty limited interface however.

A short example:

```rust
extern crate hastur;

fn main() {
  let mk = hastur::Make::new().unwrap();
  mk.eval_str("A_VARIABLE := some value").unwrap();
  let var = make
      .lookup_variable_for_file("A_VARIABLE", None as Option<String>)
      .unwrap();
  let var_contents = var.get_contents().unwrap().to_str().unwrap();
  assert_eq!(var_contents, "some value");
}
```
