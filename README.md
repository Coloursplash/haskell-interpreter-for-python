# HIPY - Haskell Interpreter for Python

HIPY is a Python interpreter written in Haskell. It aims to provide a lightweight and functional approach to executing Python code, leveraging Haskell's strong type system and purity to ensure correctness and maintainability.

## Features

- 🚧 (WIP) Parsing of Python source code
- 🚧 (WIP) Evaluation of Python expressions and statements
- 🚧 (WIP) Support for basic Python data types (integers, strings, lists, etc.)
- 🚧 (WIP) Functional and modular design in Haskell
- 🚧 (WIP) Control flow structures (if-else, loops)
- 🚧 (WIP) Function definitions and calls

## Installation

To build HIPY, you will need:

- GHC (Glasgow Haskell Compiler) >= 9.0.0  
- Cabal for dependency management  


### Using cabal

```sh
git clone https://github.com/coloursplash/hipy.git
cd hipy
cabal build
cabal run hipy
```

## Usage
HIPY runs Python scripts passed as command-line arguments:

```sh
hipy script.py
```

### Example
Create a Python script:

```python
# script.py
x = 10
y = 20
print(x + y)  # Output: 30
```

Run it with:

```python
hipy script.py
```

## License
HIPY is released under the MIT License. See LICENSE for details.