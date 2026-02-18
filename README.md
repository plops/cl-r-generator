## Primary Purpose

**cl-r-generator is a Common Lisp code generator that translates Lisp S-expressions into R programming language code.** It allows developers to write R code using Lisp syntax, which is then transformed into valid R scripts.  

## Main Functions

The codebase contains three primary top-level functions:

1. **`emit-r`** - The core code generation function that recursively translates Lisp expressions into R code  

2. **`write-source`** - Writes generated R code to a file with hash-based change detection to avoid unnecessary rewrites  

3. **`write-notebook`** - Generates Jupyter notebooks with R cells from Lisp code  

## Core Functionality

The `emit-r` function supports extensive R language constructs including:

- **Assignment operators**: `<-`, `->`, `=`, `setf`  
- **R-specific operators**: `%*%` (matrix multiplication), `%in%`, `%o%`, `%x%`, `%%`, `%/%`  
- **Data access**: `$` for accessing data frame columns, `aref` for array indexing  
- **Control flow**: `if`, `cond`, `for`, `while`, `when`, `unless`  
- **Function definitions**: `lambda`, `defun` with support for keyword parameters  
- **Formula operator**: `~` for R model formulas  
- **Function calls**: Standard function call syntax with positional and keyword arguments  

## Structure

The codebase is minimal and focused:
- **Single main module**: `rlang.lisp` containing all code generation logic
- **Examples directory**: Contains practical examples demonstrating GAM (Generalized Additive Models) analysis  
- **Helper functionality**: Includes utilities for printing floating-point numbers with sufficient precision  

## Notes

The project uses an inverted readtable case to handle R's case-sensitive naming conventions properly. 

