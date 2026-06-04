# RCC

RCC (Rust C Compiler) is a implementation of the psedocode and algorithms from Writing a C Compiler by Nora Sandler.

## Features

* **Loops** for, while, do-while
* **Variables** shadowing, assignment, declaration, initalization
* **Scope** function scope, block scope, global scope
* **Types** currently only supports int
* **Storage Class** static, extern
* **Functions** declaration, implementation, function-call syntax
* **Statements** if, if-else
* **Operators** all arithmetic, logical, and bitwise operators

### Prerequisites

Build with rustc version 1.93.0 or later
Currently only supports linux (must use WSL on Windows)

## Installation

1. Clone the repo:
```
git clone https://github.com/ethanbown/Rust-C-Compiler.git
```
2. Enter the directory:
```
cd rust-c-compiler
```
3. Build the project:
```
cargo build
```
