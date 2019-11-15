#  Compiler Deconstruction

The name of this repo is a play on the title of Niklaus Wirth's book, *Compiler Construction*.  I created it because of my utter dismay at the poor quality of the code in that book, which is supposed to be an instructional tool for teaching the basics of how to write a compiler.  

All source code should be as clearly written as possible, but in the case of educational code, clarity is even more essential, not only because its lack inhibits learning, undermining the whole point, but also because poorly written instructional code teaches bad habits by example.  

This repo is my contribution to making it clearer.  I hope you find it useful.

## Why translate it to Swift?

The original code was written in the Oberon programming language (of which Oberon-0, the language the compiler in the book translates, is a subset).   So why not just make that code better in Oberon?

First of all, while Oberon-0 possesses a good balance between simplicity and complexity to serve as the language for which to write a compiler for teaching purposes, the choice of Oberon as the implemenation language for the compiler itself was pretty lame even at the time.  Were it not for Wirth's book (or just happening on this repo) would you have even heard of it?  No one uses it, nor really have they ever.  The instructional value of the code would have been much better if it were written in a language people actually used.   For its time, Pascal, Wirth's most famous contribution to programming, would have been a good choice, since it was widely taught to computer science students not very long before he initially published *Compiler Construction* in 1996, and at least Pascal had been used as an actual production language.  For example it was the high level language of choice for early Mactinosh programming.  To be fair, Oberon is a lot like Pascal, enough that anyone familiar with Pascal could certainly read it.  I did my time programming in Pascal, and have no desire to write code in a Pascal-like language today.  C would probably have been an even better choice, since it had been in wide-spread use since the 1970s, and by the 1990s Pascal's popularity was dwindling, whereas C's remained steady.  Indeed C remains one of the most widely used programming languages even today, only recently being displaced from the #1 spot by languages like JavaScript and Python.

So why Swift and not C?  It comes down to personal preference.  I don't mind programming in C, and I've certainly written plenty of C code in my life, along with a few other languages.   I don't get bent out of shape like some people at the prospect of managing my own memory.  A lot of my professional career was writing device drivers, kernel extensions, disk utilities, and such.  I rather like programming down to the metal, and C and C++ let me do that. But I do really like Swift, despite separating me a little more than I'd like from the hardware, and though it's a relatively new language, the success of the Apple ecosystem, particularly iOS devices, means a lot of people have learned it.  In the final analysis, this translation and refactoring of Wirth's compiler is something I'm doing in my spare time.  If I'm going to write code for free, I'm going to write it in a language I actually enjoy programming in, and I really do enjoy working in Swift.

## What's with the different versions?

This repo consists of a series of directories that represent the evolution of the code in various stages.  Yes, I know that's what version control is for in the first place, but I don't want anyone to have to go back to and check out different revisions of the code just to compare major versions side-by-side. 

The directory, `Oberon-0-Original`, contains the source code in the Oberon programming language as listed in the book.  This code is not compilable unless you want to go through the trouble of installing the Oberon operating system or somehow get a native Oberon compiler for your platform, and even then some dependences on UI elements will require stubbing out or emulating Oberon libraries (like references to Oberon MenuViews).  Still, this is the literal code from the book, and so makes the best starting point if you're following the book.

The directory, `Oberon-0-Faithful`, contains a literal translation of Wirth's Oberon code into Swift.  I changed as little as possible, even emulating some Oberon types (like its fixed length arrays) with custom Swift types just to preserve the semantics of the original code.  Apart from the change in implementation language, the only other widespread change from the original is that the original was formatted fairly densely, I assume to reduce paper consumption in printing the book, and that format had a tendency to hide program structure.  That gets in the way of understanding.  The Swift code is formatted to reveal that structure more clearly without changing it.   Though insufficient by itself, just this reformatting helped the code's readability tremendously.

The directory, `Oberon-0-Swift`, contains a much more Swift-like translation of the compiler using the faithful version as a starting point.  I think you'll find this version to be much more readable, since identifier names have been drastically improved, code has been refactored so that it's easier to follow, integer values that served as enumerations have been changed to actual Swift enum types, and all references to Oberon-native and Oberon-OS types have been removed, relying on much more familiar standard Swift and Foundation types.  There is a drawback in using this code to follow Wirth's book: Because so many of the identifiers in the original code were so poorly named, it will seem as though almost everything has been renamed.  I think you'll agree that the new names are much more informative, but you'll have to do some mental mapping when reading the code in conjunction with the book.  Sorry... not sorry.  They shouldn't have been named so badly in the first place.  Suggestions for improving its clarity are welcome.

The directory, `Oberon-0-AST`, is a reworking of the parser to generate an abstract syntax tree as part of its compilation, and is a work in progress (ie. it does not use the AST for compilation purposes *yet*, though it does use it for unit testing purposes.)  While the previously described directories all contain versions that are in some way true to the original, this one begins to depart from it.  Wirth's compiler, as listed in the book, does not generate an abstract syntax tree, so this constitutes a significant departure from the single-pass design of the original.  That also means that it's no longer how Wirth is teaching to implement a compiler, but rather how I am choosing to, albeit using his compiler as a starting point.

If I have time I may add other versions.  At some point I woud like to extend it to generate actual x86-64 or ARM executables rather than just byte-code for Wirth's emulated RISC machine.  There are lots of improvements that could be made, but one has to balance that with the purpose of teaching the basics of compiler construction, not fully implementing a language that no one would actually want to use.

## Do you want to contribute?

There are definitely some opportunities for improvements, even given the limited scope of what I've done so far.  In addition to macOS, Swift is available on Ubuntu Linux, and though I have yet to successfully build it, theoretically for 64-bit Windows too, but currently I've only compiled these Oberon-0 compilers on macOS in Xcode.  It would be nice to for the projects to be readily built on other platforms where Swift has been ported, so if anyone wants to contribute build scripts, that would be awesome.

Unit tests are a bit lacking.   Of course, since the original code had no unit tests at all,  what I've added is an improvement, but it's nowhere near what it should be.

If you think my choices in refactoring could be made better, by all means please suggest alternatives, or even better, implement what you think it should be and submit a pull request.

And please do let me know if you find a bug!
