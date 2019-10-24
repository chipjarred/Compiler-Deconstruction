#  Oberon-0 Compiler

This is a faithful translation into Swift of the Oberon-0 compiler source code described in Nicklaus Wirth's book, Compiler Construction. 

## Notes on the translation
Obviously in translating the source code from Oberon to Swift, some changes were necessary.  The Oberon modules are packaged as Swift structs with static methods and variables.  I originally had used a separate framework for each Oberon module, as that seemed to best maintain the spirit of a module, but debugging was more annoying than it should have been.  Two ancilliary frameworks survive from that, `Oberon` and `Texts`, which reside in the `OberonLib` and `TextsLib` folders respectively.  The `Oberon` module provides some typealiases, extensions and structs that attempt to mimic Oberon types, to make the source look and behave as much like the original as possible.  The `Texts` module is a minimal implementation of the `Texts` module from the Oberon operating system.  It is not strictly part of the Oberon programming language, but the original source code depended on it.  Only enough of `Texts` is implemented to make the compiler work, and while some of that code is translated from actual Oberon source files, a lot of is just stubbed out or hacked to make it work. 

Maintaining as much similarity to the original Oberon code was the main priority.  The lack of comments and useful names reflect the original state of the code.  One of the main source of problems turned out to be Oberon's implicit integer conversion, which Swift doesn't do.  As a result, there are some helper functions added.  I did write my own function for writing RISC instructions in the code generator, `OSG.swift`, though the original code is still there.   Some of the functions in the original parser module, `OSP`, are tightly coupled to UI elements in the Oberon operating system.   Those are conditionally compiled out of the Swift, with modified replacements in `OSP-ModifiedInterface.swift`.

The code is very unswifty, with some design decisions, like extensive use of global variables, were questionable even for the era in which it was written.  This translation made no attempt to fix any of that.  One problem I did fix everywhere was code formatting.   The style used in the book, which may have been to reduce paper consumption in printing it, tended to hide program structure. 

I have provided a few unit tests.  There were none for the original code, and it was definitely not designed for automated testing.

## Notes on usage
This project contains command line targets, `oberonc`, which is the compiler, and `riscexec`, which loads and executes a compiled Oberon program.   These tools are very basic; however, their usage is described by executing them from the command line with no arguments. 

Currently the compiler produces two files on successful completion:

- `a.asm`  is an assembly listing (using the assembly language described in Compiler Construction)
- `a.risc` is the binary program output
	
The names of these files are chosen to mirror the default output filename of the Unix C compiler, `a.out`.
	
Although multiple modules can be in a single invocation of `oberonc` and are written to the output, at the moment, only the entry point for the last one is recorded, and it is that module that will be executed by `riscexec`, and no provision has been made to link multiple modules.   All I/O for the program being run by `riscexec` is expected to occur via `stdin` and `stdout`.
