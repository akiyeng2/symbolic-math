symbolic-math: a Lisp Symbolic Math Library
===========================================

Introduction
------------
This is a work-in-progress lisp library for symbolic math manipulation. The dialect used is Common Lisp.

Compiling & Loading
-------------------
To load this library, open a lisp interpreter and load the definitions.

If you load this library often, you may choose to compile the library by compiling the files and load the following `.fas` or `.fasl` files. Check your lisp implementation for more details.

Usage
-----
Symbolic evaluation of functions is supported by the `simplify` function.
Differentiation is supported by the `delta` function.

Supported Functions
-------------------
All elementary functions are currently implemented.