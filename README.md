# CS132 Project - Multiple Inheritance in Hasty

I have extended our Hasty compiler from Homework 9 to support multiple
inheritance (and single inheritance, by extension).

This required updating the language grammar, the parser, the typing context (to
store multiple superclasses per class), the type checker, and the
Tasty-to-Target translator.

## What is Multiple Inheritance?

Traditional single inheritance allows a class to extend a single superclass, inheriting its fields and functions. Multiple inheritance simply expands this to allow classes to inherit from multiple superclasses. Cycles of inheritance are still forbidden, meaning that a class A cannot list class B as a superclass if B ever extends from class A. However, a class might extend from a class multiple times, like if class A inherits from classes B and C, while B and C both inherit from class D.

## Diamond Problem

Suppose we have the above case, where class A inherits from classes B and C, while B and C both inherit from class D. For a more concrete example, suppose we have a Musical class that extends the Play and Opera classes, which are both subclasses of Performance. If both Play and Opera override the `perform()` method from Performance, what happens if you call `perform()` on a Musical instance? Should we use the implementation from Play or Opera? This ambiguity is called the [Diamond Problem](https://en.wikipedia.org/wiki/Multiple_inheritance#The_diamond_problem) because of the shape of the inheritance graph.

Different programming languages approach the Diamond Problem in different ways. I have taken the Python approach, where the order that a class lists its superclasses is used as a precedence for ambiguous function calls. The downside to this approach is that implementations from classes low in precedence are inaccessible to the subclass. One way to address this downfall would be to allow explicit calls to superclass methods. The [Go programming language](https://joaodlf.com/go-the-diamond-problem.html) allows this, where if you are trying to access the `perform()` method from the Opera class using a Musical instance `m`, you can explicitly write `m.Opera.perform()`.