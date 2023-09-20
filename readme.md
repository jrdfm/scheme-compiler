# Scheme Compiler Project

This project is a compiler and interpreter for a high-level functional language based on Racket. It translates Racket code into x86-64 assembly code with a C runtime system. The key accomplishments of this project include:

## Features

- Support for primitives, vectors, strings, variadic operations, and syntax validation.
- Implementation of inductively defined data types and pattern matching.
- Handling of conditionals and dispatch evaluation.
- Flexible binding for multiple expressions.
- Creation of functions with advanced features:
  - Arity-checking
  - Rest arguments
  - Arity-based dispatch
  - Tail call optimization
- Integration of lambda statements and support for multiple returns.

This repository includes two versions of the compiler. The 'loot' directory houses the more advanced version, which supports lambda statements and implements all functions using them. The other version is simpler but includes all the features, excluding anonymous functions.

## Project Structure

The project is organized as follows:

- `compiler.rkt`: The main compiler implementation in Racket.
- `compiler-file.rkt`: Compiles a racket file using `compiler.rkt`.
- `runtime.o`: The C runtime system used for executing compiled code.
- `*.s`: The generated x86-64 assembly code.
- `examples/`: Directory containing example Racket programs to compile and run.

## Usage

To compile and run a Racket program using this compiler:

1. Make sure you have Racket and a compatible C compiler (e.g., GCC) installed on your system.

2. Clone this repository:

   ```bash
   git clone https://github.com/yourusername/scheme-compiler.git
   cd scheme-compiler

Compile the Racket program using the compiler:

   ```bash
   racket -t compile-file.rkt -m 42.rkt  
```
Make sure your file has the header #lang racket followed by an expression. This will generate an 42.s file containing x86-64 assembly code.
To assemble this program into an object file, you can run the nasm assembler:
   ```bash
   nasm -f elf64 -o 42.o 42.s
```
Which will create 42.o object file which can be linked with the runtime and executed as such:
   ```bash
   gcc main.o print.o 42.o -o 42.run
   ./42.run
```
To make things easier you can use the Makefile and just run:
   ```bash
   make 42.run
   ./42.run
```