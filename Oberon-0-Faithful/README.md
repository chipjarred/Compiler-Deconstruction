#  Oberon-0 Compiler

This is a faithful translation into Swift of the Oberon-0 compiler source code described in Nicklaus Wirth's book, Compiler Construction. 

## Notes on the translation
Obviously in translating the source code from Oberon to Swift, some changes were necessary.  The Oberon modules are packaged as Swift structs with static methods and variables.  I originally had used a separate framework for each Oberon module, as that seemed to best maintain the spirit of a module, but debugging was more annoying than it should have been.  The `Support` directory contains code needed to make this translation into Swift look as much like the Wirth's original Oberon code as possible.  It includes some type definitions to emulate Oberon types and built-in functions, as well as a replacement for the Oberon operating system's `Texts` module that is used in the original code.  

Maintaining as much similarity to the original Oberon code was the main priority.  The lack of comments and useful names reflect the original state of the code.  One significant source of problems turned out to be Oberon's implicit integer conversion, which Swift doesn't do.  As a result, there are some helper functions added.  I did write my own function for writing RISC instructions in the code generator, `OSG.swift`, though the original code is still there.   Some of the procedures in the original parser, `OSP`, are tightly coupled to UI elements in the Oberon operating system.   Those are preserved but conditionally compiled out of the Swift, with modified replacements in `OSP-ModifiedInterface.swift`.

The code is very "unswifty."  This translation made no attempt to fix any design decisions I feel were poor choices, though one problem I did fix everywhere was code formatting.   The style used in the book, which may have been to reduce paper consumption in printing it, tended to hide program structure.   The resulting Swift code contains more literal lines of code simply because it doesn't place multiple statements on a single line.  This reveals more of the program structure that is there in the original, but hidden by the dense formatting.

I have provided a few unit tests, though not nearly as many as there should be.  There were none for the original code, and it was definitely not designed for automated testing, so adding thorough unit testing is a real pain.

## Notes on usage
This project contains command line targets, `oberonc`, which is the compiler, and `riscexec`, which loads and executes a compiled Oberon program in a emulator for the RISC machine described by Wirth.   These tools are very basic; however, their usage is described by executing them from the command line with no arguments. 

Currently the compiler produces two files on successful completion:

- `a.asm`  is an assembly listing (using the assembly language described in *Compiler Construction*)
- `a.risc` is the binary program output
	
The names of these files are chosen to mirror the default output filename of the Unix C compiler, `a.out`.
	
Although multiple modules can be compiled in a single invocation of `oberonc` and are written to the output, at the moment, only the entry point for the last one is recorded, and it is that module that will be executed by `riscexec`, with no provision has been made to link multiple modules.  While this may seem like a major limitation, and it is, the Oberon-0 language being compiled is itself very limited anyway, and I felt this was sufficient for educational purporses.    All I/O for the program being run by `riscexec` is expected to occur via `stdin` and `stdout`.
