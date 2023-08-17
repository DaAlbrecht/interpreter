# Rust Interpreter for "Writing An Interpreter In Go"

## Table of Contents

- [Rust Interpreter for "Writing An Interpreter In Go"](#rust-interpreter-for-writing-an-interpreter-in-go)
  - [Table of Contents](#table-of-contents)
  - [Motivation](#motivation)
  - [Features](#features)
  - [Getting Started](#getting-started)

This repository contains a Rust implementation of the book [**"Writing An Interpreter In Go"**](https://interpreterbook.com/) by Thorsten Ball. The book walks through the process of building an interpreter for a simple programming language using the Go programming language. In this repository, I aim to replicate the interpreter using Rust.

## Motivation

The primary motivation behind this project is to learn about interpreters and gain a deeper understanding of the concepts and techniques taught in the book.

## Features

- **Lexer:** Tokenizes input source code into meaningful tokens.
- **Parser:** Parses the tokens into an abstract syntax tree (AST).
- **Evaluator:** Evaluates the AST and performs computations according to the programming language's rules.

## Getting Started

To run the interpreter, follow these steps:

1. Clone this repository to your local machine.

```bash
git clone https://github.com/DaAlbrecht/interpreter.git
```
2. Navigate to the project directory.

```bash
cd interpreter
```

3. Build and run the interpreter.

```bash
cargo run
```

