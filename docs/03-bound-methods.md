# Step 3: Bound method calls

In this part of the assignment, we will add support for "bound" method calls.
A bound method is one whose first argument is an instance of the class. The
class constructor and all non-static methods of the class are bound. 

I use the term "bound" to distinguish from virtual methods. A virtual method is
a bound method that can be inherited and possibly overridden, and it is called
via dynamic dispatch. We are not implementing inheritance or dynamic dispatch,
so we will restrict ourselves to implementing bound methods.

## Add the feature

To add this feature, we need to distinguish between an unbound (i.e., static) and
a bound method, both when a methods is declared, and when the method is called.

When the method is declared, we need to add the `self` parameter to the method
declaration. When the method is called, we need to pass the receiver of the
method as the first argument.

Implement bound methods, by making the appropriate changes to method
declarations in `src/Analysis/Typecheck.hs`. Similarly, implement calls by
providing support for `EInvoke` in `src/Analysis/Typecheck.hs`.

After you have completed your implementation, you should be able to successfully
do the following:

```
./compile_and_run programs/methods1.hasty
./compile_and_run programs/methods2.hasty
./compile_and_run programs/methods3.hasty
./compile_and_run programs/fact9.hasty
```

## Hints / recommendations
- If your code did not previously distinguish between static and non-static
  methods, it will probably have to do so now.

- Note that we do _not_ need to modify `src/IR/Translate.hs`, because we can turn
  Hasty bound calls (`H.EInvoke`) into regular Tasty calls (`T.ECall`)...

- Similarly, we can ignore any information about the method's "number", which is
  information we would need if we were implementing dynamic dispatch.

