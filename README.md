# pasta

A dependently typed programming language with primitive product and sum types.

```
foo : (Int, String) -> Int
foo x = x @ 0

bar : (Int | Bool) -> String
bar (@ 0 _) = "An Int!"
bar (@ 1 _) = "A Bool!"
```
