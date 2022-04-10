# Step 1: Static methods
In this part of the assignment, we will add support for static methods. 

To do so, we will need to consider the following aspects:

For **class declarations**:
  - How to typecheck static methods.
  - How to translate the Hasty declaration to a Tasty declaration.
  - How to translate the Tasty declaration to a Target declaration.

For **static method calls**
  - How to typecheck the call.
  - How to translate the Hasty call to a Tasty call.
  - How to translate the Tasty call to a Target call.

Let's begin!

## Class declarations
1. To typecheck and translate class declarations, write code for `checkMethod` in
   `src/Analysis/Typecheck.hs`. You will probably need to (re-)familiarize
   yourself with Hasty and Tasty IR.
1. To translate the Tasty declaration to a Target declaration, write code for `xMethod` in
   `src/IR/Translate.hs`. You will probably need to (re-)familiarize
   yourself with Target IR.

### Hints
- Throughout the assignment, you may want to refer to `src/IR/Hasty.hs`,
  `src/IR/Tasty.hs`, and `src/IR/Target.hs`, and be familiar with the all the
  information that the context can provide.

- It is okay to ignore the `MethodKind` for now and assume that all methods are
  static. However, if you would like to specialize `checkMethod` to work only
  for `StaticMethods`, you can. 

- Notice that the translate step (in `xMethod`) "mangles" the method name, to
  include the class name. For typechecking, you can use just the method name.

- If you make good use of the helper functions already defined for declarations,
  then it is easy to typecheck and translate a static-method declaration. Most
  of the code is already written for you! You can take inspiration from the
  implementations for function declarations...

## Static methods calls
1. Implement typechecking, translation to Tasty, and translation to Target for
   static method calls (`EStaticCall`).

After you have completed your implementation, you should be able to successfully
do the following:
```
./compile_and_run programs/static1.hasty
./compile_and_run programs/static2.hasty
```

### Hints
- As with the previous task, helper functions are your friend!! I ended up
  writing four lines of code to implement static methods calls. Most of the work
  was understanding what the compiler is supposed to do, and figuring out how to
  get and transform the correct information, via helper functions. Also,
  remember that the context has information about the class's static methods.