# Assignment 9: Object-Oriented Hasty

As the final step in the compilers project, we are going to add classes and
objects (but not inheritance). This will involve extending the type checker (to
validate object-oriented programs), and the Tasty-to-Target translator (to
translate the new object-oriented features).

## Getting Started

The starter code for this assignment is a "reset": It contains the sample
solution for the previous assignment. I recommend that you start with this code,
_but_, there may some important differences between this code and your code,
including the choices for encoding strings and optionals. The benefit of using
this code is that is a (hopefully!) working version of the compiler, and the
code is relatively simple and easy to understand. The downside is that it wasn't
written by you, so you may be less familiar with it. If you want, you can start
from your HW 8 code, but let me know, so I can help you.

## Instructions

0. [Part 0: Records](docs/00-records.md)
1. [Part 1: Static methods](docs/01-static-methods.md)
1. [Part 2: Object instantiation](docs/02-instantiation.md)
1. [Part 3: Bound methods](docs/03-bound-methods.md)
1. [Part 4: Write an interesting program](docs/04-example.md)
1. [Submit](docs/05-submit.md)

Note: Although we are not implementing support for inheritance or dynamic
dispatch, the syntactic features are implemented (i.e., we can parse programs
that use inheritance), and the implementation has a few helper functions that
will be useful for implementing these features. If you finish everything else,
and want to implement inheritance and dynamic dispatch, go for it!