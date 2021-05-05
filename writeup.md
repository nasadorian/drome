Narek Asadorian \
CSCI E-51 \
Spring 2021 \

---

# drome: The IO Monad for OCaml

## Abstract

OCaml sits at a "sweet spot" in the world of programming languages as it provides support for multiple programming paradigms, a powerful type system and novel abstractive capabilities. It is a friendly functional language well suited for beginners, but makes a tradeoff to achieve this status - it is neither pure nor lazy by default like its more advanced sister language Haskell. In an attempt to bridge this gap, we offer an implementation of the IO monad and related effect management utilities for OCaml.

Jane Street has addressed the effect system problem for OCaml by means of its algebraic effects library (White et. al. 2018, https://www.janestreet.com/tech-talks/effective-programming/). The OCaml ecosystem however seems to be missing a monadic effects library, possibly due to the lack of higher kinded types in the language. Despite this limitationn, we seek to demonstrate that the monadic approach to effect management can be implemented in a sane and user-friendly manner in OCaml. Given the presence of `option`, `result` and `either` monads (and associated combinators) in the standard library, an IO monad can fit directly into the common functional programming style supported by the language. Further, we posit that monadic effects more naturally fit into the OCaml programming style than the callback-and-continuation algebraic style which has an arguably much steeper learning curve.

Our library is named `drome`, after the versatile and hard-working Dromedary camel widely used in the African content. The `drome` library takes inspiration and cues from the effects system for Scala called `cats-effect` (Spiewak et. al., https://github.com/typelevel/cats-effect). The approach taken in the library centers around a domain specific language for lazy IO computations embedded in OCaml as the host language. XXX The IO "language" itself can be executed by a various target interpreters including a synchronous one and potentially an asynchronous one if time permits.

## Library Overview

## The IO Monad

## Resource

## RefIO

## Runtime Implementation

## Demos
