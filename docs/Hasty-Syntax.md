# Tasty: Syntax and Semantics


## Tokens

* Comments start from the `//` marker to the end of line.
* The boolean constants are `true` and `false`
* Integer constants are nonnegative, and are either written in decimal (e.g., `42`, `007`, with leading zeros allowed) or hexadecimal (e.g., `0x2A` or `0xff`, where the digits can be lower or upper-case, but the `x` must be lower-case).
* Variable and function names consist of a lowercase letter followed by zero or more letters, numbers and underscores.
* Other than separating tokens from each other (e.g., `ifflag` vs. `if flag`), whilespace, indentation, and newlines do not affect the meaning of code.

## Grammar  (with comments)

```text
<Type> ::=  Int         // Type of 64-bit ints
         |  Bool        // Type of Booleans
         |  String      // Type of Strings
         |  <Type> ?    // Optionals
         |  Void        // Absence of a value

<Expression> ::=  <integer constant>
               |  <boolean constant>
               |  <string constant>
               |  <identifier>
               |  - <Expression>
               |  ! <Expression>

               |  <Expression> + <Expression>            // Integer arithmetic
               |  <Expression> - <Expression>
               |  <Expression> * <Expression>
               |  <Expression> / <Expression>

               |  <Expression> == <Expression>           // Integer comparisons
               |  <Expression> != <Expression>           //
               |  <Expression> <  <Expression>           //
               |  <Expression> <= <Expression>           //
               |  <Expression> >  <Expression>           //
               |  <Expression> >= <Expression>           //

               |  <Expression> && <Expression>           // Logical operations on booleans
               |  <Expression> || <Expression>           //   (&& and || are short-circuiting)
               |  ! <Expression>

               |  <Expression> . <identifier>            // Projection

               |  <Type>(<Expression>)                   // Type casting

               |  nil                                    // empty optional value
               |  <Expression> ??  <Expression>          // "Nil-coalescing" operator

               |  <Expression> ? <Expression> : <Expression>   // Ternary operator

               |  <identifier> ( <Expressionz> )   // Call of value-returning function

               |  ( expr )       // Grouping parentheses

               | <ClassName> ( <Expressionz> )  // Create new object
               | <Expression> . <identifier> ( <Expressionz> )  // Method Invocation

 //  <Expressionz> is zero or more comma-separated expressions
 //  <Expressions> is one or more comma-separated expressions
 //      Defining the former in terms of the latter makes it *much* easier
 //      to rule out leading/trailing commas
 //      (i.e., "1,2,3" is ok but "1,2,3," and ",1,2,3" and "," are not)


 <Expressionz> ::= ε
                 | <Expressions>

 <Expressions> ::= <Expression>
                 | <Expression> , <Expressions>

 <Statement> ::=  if <Expression> <Block>                   // By requiring a <block> here, we
               |  if <Expression> <Block> else <Statement>  // avoid a shift-reduce conflict!
               |  <Block>
               |  while <expression> <Block>           // While loop
               |  <identifier> ( <Expressionz> ) ;     // Function call (ignoring return value)
               |  return <Expression> ;
               |  return ;
               |  print ( <Type> , <Expression> ) ;
               |  <LValue> = <Expression> ;
               |  var <identifier> : <Type> = <Expression> ;    // Variable declaration
               |  <Expression> ;                      // for its side-effects

 <LValue> ::= <identifier>
            | <Expression> . <identifier>

 <Block> ::= { <Statements> }

 // When we have a list of things with no separator, we
 //  don't need to go to the trouble of defining <Statementz> and <Statements>

 <Statements> ::= ε
                | <Statement> <Statements>

 // Again, we define 0-or-more function parameters
 // in terms of 1-or-more parameters, to get the commas right.

 <Parameterz> ::= ε
               | <Parameters>

 <Parameters> ::= <identifier> : <Type>
               | <identifier> : <Type> , <Parameters>

 <Declaration> ::= func <identifier> ( <Parameterz> ) -> <Type> <Block>
                 | class <classname> <Superclass> { <Fieldz> <Constructor> <Methodz> }

 <Declarationz> ::= ε
               | <Declaration> <Declarationz>

 <Superclass> ::= ε
                | : <classname>

 <Field> ::= var  <identifier> : <Type> ;

 <Fieldz> ::= ε
            | <Field> <Fieldz>

 <Constructor> ::= init ( <Parameterz> ) <SuperInit> <Block>

 <SuperInit> ::= ε
               | : super ( <Expressionz> )

 <Method> ::= <MethodKind> func <identifier> ( <Parameterz> ) -> <Type> Block

 <MethodKind> ::= ε
                | override
                | static

 <Methodz> ::= ε
            | <Method> <Methodz>

 <Program> ::= <Declarationz>
```

### Precedence

Precedence in Tasty is closer to that of Swift than of C. In order of increasing precedence, the operators are:

* Right-associative: `?` and `:`
* Left-associative: `||`
* Left-associative: `&&`
* Not associative (`%nonassoc`):  `==`, `!=`, `<`, `>`, `<=`, `>=`
* Right-associative: `??`
* Left-associative: `+` and (binary) `-`
* Left-associative: `*` and `/`
* Not associative: `!` and unary `-`
* Left-associative: `.`
