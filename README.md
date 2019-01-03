HASTUR
======

Hastur is a Rust library for interacting with makefiles. Eventually, you should
be able to extract most pieces of information from any given `Makefile` and
interactively explore the set of variables and definitions that result after
evaluation.

Hastur also aims to track the sensitivity of the Makefile to the value of any variables,
in order to facilitate interactively exploring the effects of variable values
as well as explain to the user how certain variables came to have their current values.
