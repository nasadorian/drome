Narek Asadorian \
CSCI E-51 \
Spring 2021 \

---

# drome: The IO Monad for OCaml

## Abstract

OCaml sits at a "sweet spot" in the world of programming languages as it provides support for multiple programming paradigms, a powerful type system and novel abstractive capabilities. It is a friendly functional language well suited for beginners, but makes a tradeoff to achieve this status - it is neither pure nor lazy by default like its more advanced sister language Haskell. In an attempt to bridge this gap, we offer an implementation of the IO monad and related effect management utilities for OCaml.

Jane Street has previously addressed the effect system problem for OCaml by means of its algebraic effects library (White et. al. 2018, https://www.janestreet.com/tech-talks/effective-programming/). The OCaml ecosystem however seems to be missing a monadic option for effects, possibly due to the lack of higher kinded type abstraction in the language. Despite this limitation, we seek to demonstrate that the monadic approach to effect management can be implemented in a sane and user-friendly manner in OCaml. Given the presence of `option`, `result` and `either` monads (and their associated combinators) in the standard library, there is certainly room for an IO monad in the common functional programming style supported by OCaml. Further, we posit that monadic effects more naturally fit into the OCaml programming style than the callback-and-continuation algebraic style which also has an arguably steeper and more mind-bending learning curve.

Our library is named `drome`, after the versatile and hard-working Dromedary camel widely used on the African content. The `drome` library takes its inspiration and design cues from the effects system for Scala called `cats-effect` (Spiewak et. al., https://github.com/typelevel/cats-effect). The approach taken in the library centers around a domain specific language for representing deferred IO computations embedded in OCaml as the host language. The IO data type in `drome` is intended to capture singular, side-effecting actions as pure values. These values can then be composed and manipulated into larger programs through the use of functional combinators and executed by synchronous or asynchronous target interpreters.

## Library Overview

The starting point when developing `drome` programs is a composite module aptly named `Drome` which combines the `IO`, `Resource` and `RefIO` modules under one namespace. We'll begin our tour of the library with the canonical "hello world" example using the `IO` module.

```ocaml
open Drome

let hello = IO.make (fun _ -> print_endline "Hello world!");;

IO.unsafe_run_sync hello;;
(* Hello world!" *)


```

## The IO Monad

## Resource

## RefIO

## Runtime Implementation

## Demos
