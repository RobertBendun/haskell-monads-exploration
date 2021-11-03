# Haskell Monads exploration

This repo contains exploration of `State`, `Reader`, `Writer` and `RWS` monads in Haskell, due to collaboration on the [university](https://amu.edu.pl/en).

## What's interesting here?

Due to development of [Stacky](https://github.com/RobertBendun/stacky), my example for `ReadWriteState` Monad is simple concatenative stack-based language bytecode interpreter (+ compiler). You can find it in [rws-stack-based-language.hs](./rws-stack-based-language.hs). Additionaly it uses [lenses](https://hackage.haskell.org/package/lens).

## File naming convention

`<monad>-<example>.hs` is a file where I explore monad `<monad>` by modeling `<example>`

## Dependencies

Only lenses, why there are not part of standard I dunno.

```console
$ sudo pacman -S ghc ghc-libs ghc-static ghc-lens
```
