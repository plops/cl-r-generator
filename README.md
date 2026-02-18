# cl-r-generator

A Common Lisp library for generating R code from Lisp s-expressions.

## Overview

`cl-r-generator` is a code generation tool that allows you to write R statistical computing code using Common Lisp's s-expression syntax. The library transpiles Lisp forms into R code, making it easier to programmatically generate R scripts and Jupyter notebooks.  

## Features

- **R Code Generation**: Convert Lisp s-expressions to R code with the `emit-r` function
- **Jupyter Notebook Support**: Generate R-based Jupyter notebooks programmatically with `write-notebook`
- **Source File Management**: Automatically write R source files with hash-based change detection to avoid unnecessary rewrites
- **Rich Syntax Support**: Comprehensive mapping of R operators and constructs including:
  - Assignment operators (`<-`, `=`, `->`)
  - Mathematical operators (`+`, `-`, `*`, `/`, `^`)
  - R-specific operators (`%*%`, `%x%`, `%%`, `%/%`, `%o%`, `%in%`)
  - Control flow (`if`, `for`, `while`, `cond`)
  - Function definitions with lambda lists
  - Data access (`$`, `aref`)  

## Installation

### R Installation (Fedora)  

### Common Lisp Setup

Load the library using Quicklisp:

```lisp
(ql:quickload "cl-r-generator")
(in-package :cl-r-generator)
```  

## Usage

### Important: Readtable Configuration

The library requires an inverted readtable for correct symbol and filename case handling:  

### Generating R Source Files

Use `write-source` to generate R script files from Lisp code:  

### Example Usage

The library excels at generating statistical analysis code, particularly for Generalized Additive Models (GAMs):  

### Creating Jupyter Notebooks

Generate R-based Jupyter notebooks with markdown and code cells:  

## Examples

The repository includes several examples demonstrating different use cases:

- `example/01_gam/` - Generalized Additive Models analysis
- `example/03_thinplatespline/` - Thin plate spline demonstrations
- `example/04_lidar/` - LIDAR data analysis
- `example/05_ptc/` - PTC examples
- `example/06_color/` - Color-related computations

## R Language References

The implementation follows these R language specifications:  

## Assignment Operator Notes

The library consistently uses the `<-` operator for assignment in generated R code:  

