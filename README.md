# System of Linear Equations

A simple Haskell project to solve systems of linear equations using the matrix inverse method.

## Overview

This project provides a solution to systems of linear equations represented in matrix form:

**\[ A X = B \]**

Where:
- **\( A \)**: The coefficient matrix, representing the linear equations.
- **\( X \)**: The column vector of unknown variables.
- **\( B \)**: The column vector of constants.

By calculating the inverse of \( A \), the solution \( X \) can be derived as:

**\[ X = A^{-1} B \]**

## Features
- Multiplies \( A^{-1} \) with \( B \) to obtain the solution vector \( X \).
- Provides error handling for cases where the matrix \( A \) is not invertible or input is invalid.

##     Requirements

* GHC (Glasgow Haskell Compiler): Ensure you have GHC installed. 
* Cabal: For managing Haskell dependencies and building the project.

## Run Locally

Clone the repository and navigate to the project directory:

```bash

git clone https://github.com/HJyup/system-of-linear-equations.git
cd sudoku-solver

```

Build the project with Cabal:

```bash

cabal build

```

Run the application with Cabal:

```bash

cabal run system-of-linear-equations

```


## License

[MIT](https://choosealicense.com/licenses/mit/)
