# Crafting an Interpreter

This is my code that I wrote while reading the book [Crafting Interpreters](https://craftinginterpreters.com/).

It differs in a few aspects:
* I'm using D (of course), not Java or C
* I'm trying to utilize my [iopipe library](https://github.com/schveiguy/iopipe) as a test to see how usable it is.

I'm hoping that I get a greater sense of language design out of this exercise, and also get a better sense of how the iopipe API must change for the better.

The phase1 directory is the "Java" tree-walk interpreter.

The phase2 directory is the "C" bytecode interpreter.
