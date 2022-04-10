# Step 2: Object instantiation
In this part of the assignment, we will add support for instantiating objects:
allocating space for the object and calling its constructor to initialize its fields.

## Add the feature
1. Add support for **constructor declarations**, in the appropriate places
   in `src/Analysis/Typecheck.hs` and `src/IR/Translate.hs`.
1. Add support for **object instantiation (`ENew`)**, in the appropriate places
   in `src/Analysis/Typecheck.hs` and `src/IR/Translate.hs`.   

In all cases, you can **ignore the superclass initializer** because we are not
implementing inheritance.

After you have completed your implementation, you should be able to successfully
do the following:

```
./compile_and_run programs/new1.hasty
./compile_and_run programs/new2.hasty
```

### Hints / Recommendations
- If you ever need a name for the constructor, use `ClassName__init`.
- Constructors don't have a return type, but some helper functions in our
  implementation expect to receive a return type. If you use these helper
  functions, use `VoidTy` as a return type.
- As always, helper functions are your friends! Remember that the context has
  information about the class's constructor and fields.
- The most difficult part of this task will probably be translating `Tasty.ENew`
  to target code. Here are a few recommendations:
    - Remember that objects are similar to records. You can look at
      the code for record literals, for inspiration.
    - Remember that object instantiation should always call the class's
      constructor, and remember that the first argument to the constructor is
      the object itself!
    - Be thoughtful about _which_ part of the code is responsible for doing the
      allocation and the constructor call!
