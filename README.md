# Scurry
[![Build status](https://github.com/dzfrias/scurry/actions/workflows/ci.yml/badge.svg)](https://github.com/dzfrias/scurry/actions)
[![Crates.io](https://img.shields.io/crates/v/scurry.svg)](https://crates.io/crates/scurry)

Scurry is an dynamically typed component-based object-oriented language,
written in [Rust](https://github.com/rust-lang/rust). Scurry is still under
active development and thus more features will be available soon.

...

That was a mouthful. So what can it actually do?

## Overview
Here are the key design philosophies of Scurry:
- **Strictly component-based.** Objects in Scurry **only** support
  component-based design
- **Limits mutable state.** The objects that own the state are the only ones
  that can mutate it directly.
- **Intuitive.** Scurry's syntax is predictable and minimal. Think Python with
  some elements of C.
- **Method-based.** Scurry encourages clean method-based APIs in small, reusable
  components.

## Documentation
Official documentation and a tutorial is coming soon!

## Installation
Currently, Scurry can be downloaded with the following methods.

### Cargo
Must have the Rust toolchain installed.

```
$ cargo install scurry
```

### Manual
Must have the Rust toolchain installed.

```zsh
$ git clone https://github.com/dzfrias/scurry
$ cd scurry
$ cargo build --release
$ # Now put the binary wherever you'd like!
```

### License
Scurry is licensed under the MIT License.
