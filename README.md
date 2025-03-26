# Calculator_Interpreter_2

## Introduction

This project implements a simple calculator that supports basic arithmetic operations and variable assignments. The calculator is implemented in multiple programming languages, including Ada, Prolog, Perl, Racket, and Rust.

## Implementation Details

Each implementation provides a command-line interface where users can enter arithmetic expressions and assign values to variables. The calculator supports the following features:

- Addition (+), subtraction (-), multiplication (\*), and division (/)
- Parentheses for operation precedence
- Variable assignment and retrieval
- Error handling for invalid expressions and division by zero

## Language-Specific Installations

```sh
# Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Ada
sudo apt install gnat

# Perl (usually pre-installed)
sudo apt install perl

# Racket
sudo apt install racket

# Prolog
sudo apt install swi-prolog
```

## How to Run

### Rust Implementation

1. Ensure Rust is installed:
   ```sh
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   ```
2. Navigate to the Rust project:
   ```sh
   cd rust_calculator
   ```
3. Build and run:
   ```sh
   cargo run
   ```

### Ada Implementation

1. Ensure you have an Ada compiler (such as GNAT) installed.
2. Compile the `calculator.adb` file:
   ```sh
   gnatmake calculator.adb
   ```
3. Run the compiled program:
   ```sh
   ./calculator
   ```

### Perl Implementation

1. Open a terminal.
2. Navigate to the directory containing `calculator.pl`.
3. Run the script using Perl:
   ```sh
   perl calculator.pl
   ```
4. Enter expressions or assign variables as needed.
5. Type `q` to exit.

### Prolog Implementation

1. Install SWI-Prolog:
   ```sh
   sudo apt install swi-prolog
   ```
2. Run the program:
   ```sh
   swipl -q -s calculator.pro -t main
   ```
3. Ensure Rust is installed:
   ```sh
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   ```
4. Navigate to the Rust project:
   ```sh
   cd rust_calculator
   ```
5. Build and run:
   ```sh
   cargo run
   ```

## Error Handling

The calculator includes various error checks, such as:

- Detecting invalid characters in input
- Handling mismatched parentheses
- Preventing division by zero
- Checking for undefined variable usage
