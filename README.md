# Primary Purpose

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

# Examples

The **cl-r-generator** project contains five example directories, each demonstrating different applications of the R code generator:

## 1. `01_gam` - Generalized Additive Models

This example demonstrates statistical modeling using GAMs (Generalized Additive Models). It includes two generator files:

- **gen01.lisp**: Implements examples from Simon Wood's "Generalized Additive Models" book, Chapter 7 (GAMs in Practice). It covers brain imaging data analysis using Gamma distributions, interaction smooths, and model comparison using AIC.  

- **gen02.lisp**: Implements examples from Chapter 4 of the same book, focusing on piecewise linear fitting using tent functions and penalized regression splines with generalized cross-validation (GCV) for smoothing parameter selection.  

## 2. `03_thinplatespline` - Image Processing with Thin Plate Splines

This example demonstrates fitting thin plate splines to image data. It shows how to threshold a grayscale image to select wall pixels (separating them from a black cupboard) and fit a thin plate spline model to the selected pixels, including residual analysis.  

## 3. `04_lidar` - 3D Point Cloud Processing

This example focuses on processing Lidar data of indoor environments. It demonstrates how to load point cloud data of a room containing furniture (plant, table, walls), fit planes to flat surfaces using the lidR package, and classify points based on their elevation relative to the fitted planes.  

## 4. `05_ptc` - Photon Transfer Curve Analysis

This example covers camera characterization through Photon Transfer Curve (PTC) analysis. It includes two scenarios:
- Computing PTC from multiple dark and bright images to determine conversion gain, read noise in electrons, and full well capacity
- Analyzing PTC from non-uniform images with defocused edges using sigmoid function fitting and quasi-Poisson noise modeling
- Color calibration using ColorChecker targets under different illuminants (D50 and D65)  

## 5. `06_color` - Color Science and Colorimetry

This example demonstrates color science applications, specifically working with CIE 1931 chromaticity coordinates and standard illuminants (D65). It shows how to load and visualize spectral data for color analysis.  

## Notes

- Examples `01_gam` and `06_color` contain Lisp generator files that programmatically create R code
- Examples `03_thinplatespline`, `04_lidar`, and `05_ptc` contain conversation markdown files that document R code examples, likely from AI-assisted conversations
- The project uses an inverted readtable for proper case handling of symbols and filenames 




