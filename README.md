# metaocaml-letrec: flexible 'let rec' generation

In typed MetaML-based systems such as MetaOCaml it is difficult or impossible to generate mutually-recursive binding groups whose size is not known in advance.  For example, suppose (following an example of Neil Jones expounded by [Oleg][oleg]) that you want to [specialize the Ackermann function][oleg-ltu-ackermann-comment]

```ocaml
let rec ack m n =
   if m = 0 then n+1 else
   if n = 0 then ack (m-1) 1 else
   ack (m-1) (ack m (n-1))
```

with the first argument equal to `2`.  Ideally, you might like to generate the following code, with three mutually recursive bindings and all the recursive calls specialized:

```ocaml
let rec ack_2 n = if n = 0 then ack_1 1 else  ack_1 (ack_2 (n-1))
    and ack_1 n =  if n = 0 then ack_0 1 else  ack_0 (ack_1 (n-1))
    and ack_0 n =  n+1
```

With `metaocaml-letrec` you can generate exactly that code, modulo naming, by [annotating the original definition of `ack` as follows](lib_test/ackermann.ml) (and passing the argument `2`):

```ocaml
  let%staged rec ack m =
    .< fun n -> .~(if m = 0 then .<n+1>. else
                .< if n = 0 then .~(ack (m-1)) 1 else
                   .~(ack (m-1)) (.~(ack m) (n-1)) >.)>.
```

and, in general, `ack n` will generate a `let rec` group of `n+1` bindings.

More generally, `metocaml-letrec` treats a `let rec` binding as an indexed family, where the argument to the generating function is the index.  In the `ack` example, the index `m` is a simple integer; in general it may be a richer object, making it possible to generate arbitrary patterns of recursion, including

* [nested let rec bindings](lib_test/nested.ml)
* [polymorphic recursion](lib_test/polymorphic.ml)
* [recursion with non-function values](lib_test/recursive_values.ml)
* recursion where the bindings have different types

and many more examples.

### Installation

`metaocaml-letrec` currently requires [BER MetaOCaml N104][ber-metaocaml], which is [available as an `OPAM` switch][metaocaml-switch]:

```
opam switch 4.04.0+BER
eval $(opam config env)
```

Within this `4.04.0+BER` switch the `metaocaml-letrec` package can be installed as follows:

```
opam pin add letrec git@github.com:yallop/metaocaml-letrec.git`
```


[oleg-ltu-ackermann-comment]: http://lambda-the-ultimate.org/node/4039#comment-61431
[oleg]: http://okmij.org/ftp/
[ber-metaocaml]: http://okmij.org/ftp/ML/MetaOCaml.html
[metaocaml-switch]: https://github.com/ocaml/opam-repository/tree/master/compilers/4.04.0/4.04.0%2BBER
