# The C-Flat Programing Language Compiler

## A rust based compiler, for a rust inspired programing launguage.

The goal of this project is creating a low-level, statically typed programing language, that allows for fast, optimized code,
while also staying memory-safe, and incorporating a user friendly high-level syntax.
The launguage is heavily inspired by rust, including features like:
* Ownership
* Trait based type system
* Union-like enums
* Type inference
* Automatic dereferencing
* Zero sized types

## How to run the compiler

1. Make sure rust is installed on your machine (preferably linux, this project uses LLVM which is much easier to install on Linux)
2. Run 'cargo build' in the terminal to install the dependencies
3. Run 'cargo run' in the main project folder

## The script itself

Currently the compiler will compile 'src/test/main.cf' for testing, you can change that path at 'src/main.rs'