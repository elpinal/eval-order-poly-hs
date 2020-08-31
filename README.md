# eval-order-poly (Haskell)

Evaluation-order polymorphism, written in Haskell.

## Sample

`tail` function for *both* lists and streams:

```
(Λ'a -> λx ->
  match x with
    | nil -> inl nil
    | p   -> snd p
  end)
: Д'A. ∀'a. (μ'b. 'A |> (1 + 'a * 'b)) -> (μ'b. 'A |> (1 + 'a * 'b))
```

## Reference

Joshua Dunfield.
Elaborating Evaluation-Order Polymorphism.
ICFP 2015.

https://doi.org/10.1145/2784731.2784744

## TODO

1. Follow ["Let arguments go first"](https://doi.org/10.1007/978-3-319-89884-1_10) [ESOP 2018] to support the `EДElim` rule
2. Implement translation
3. Check contractiveness of recursive types
4. Support type and value definitions
