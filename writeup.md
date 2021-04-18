Narek Asadorian \
CSCI E-51, Spring 2021

---

# drome: monadic effects for OCaml 

## Abstract
<!--TODO: Rewrite proposal as abstract-->

## Milestones

* April 26th, 2021
  * IO
    - [ ] Complete unsafe synchronous DSL implementation
    - [ ] Add delay and error handling capabilities
    - [ ] Preprocessor directive for `defer`
    - [ ] Begin writing tests
  * Resource
    - [ ] Sketch out Resource DSL signature
  * Demo program
    - [ ] Determine idea to implement with IO and Resource
  * Typeclass
    - [ ] Can combinator functionality (bind, map, etc) be captured in shared sig? Do not need a fully fledged typeclass hierarchy.

* May 1, 2021
  * IO
    - [ ] Complete asynchronous DSL interpreter (priority TBD)
    - [ ] Complete test suite
    - [ ] Optimizations (map fusion, tail recursive interpreter)
    - [ ] More combinators (`ap`, `ap2`, `zip`, etc)
  * Resource
    - [ ] Complete DSL, including functions `make` & `use`. Interpreter
        should target `IO` runtime.
    - [ ] Test suite
  * Demo program
    - [ ] Begin writing
  * Project writeup
    - [ ] Write outline
    - [ ] Rewrite abstract

* May 5, 2021
  * Demo program
    - [ ] Complete and working
  * Project writeup
    - [ ] Complete writing 

## Demo Program

__Read and print loop with IO__

```ocaml
  (* No need to defer *)
  let read : string io = IO.lift read_line
  (* PPX should turn `print_string s` into `fun _ -> print_string s` *)
  let print (s : string) : unit io = IO.defer (print_string s)
  
  (* Combine read and print infinitely using bind and productL *)
  let rec loop : unit io =
    IO.( (read >>= print) *> loop )

  (* Run synchronously *)
  utop # IO.unsafe_run_sync loop
  <in> hi
  hi
  <in> there
  there
  <Ctrl-C>
```
