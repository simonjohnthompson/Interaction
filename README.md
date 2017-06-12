# Interaction
Lazy interactions in Haskell

Code for the presentation at [ZuriHac2017](https://zurihac.info).

## Note

Thanks to [Neil Mitchell](http://ndmitchell.com) for reminding me that pattern bindings in 
`where` and `let` constructs are automatically irrefutable. So, the only tilde needed is
in the definition of `make_Output`, and that can be eliminated by redefining `sq` like this:

```haskell
sq :: Interact a b -> Interact b c -> Interact a c

sq inter1 inter2 x
      = (rest2,st2,out1++out2)
    where (rest,st,out1) = inter1 x
          (rest2,st2,out2) = inter2 (rest,st)
```