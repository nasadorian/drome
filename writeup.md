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

## Demo Programs

__Echo loop with IO__

```ocaml
  (* No need to defer since `read_line` already has type `unit -> string`  *)
  let read : string io = IO.lift read_line
  (* PPX should turn `print_string s` into `fun _ -> print_string s` *)
  let print (s : string) : unit io = IO.suspend (print_string s)
  
  (* Combine read and print infinitely using bind and productL *)
  let rec echo : unit io =
    IO.( (read >>= print) *> loop )

  (* Run synchronously *)
  utop # IO.unsafe_run_sync echo
  <in> hi
  hi
  <in> there
  there
  <Ctrl-C>
```

__Pure functional retry__

```ocaml
  open Cohttp_lwt_unix;;

  (* GET a URL and convert its status to an integer *)
  let status_of_url (url : string) : int io =
    IO.suspend (
      Client.get (Uri.of_string url)
        |> Lwt_main.run
        |> fst
        |> Response.status
        |> Code.code_of_status
    )

  (* Handle errors using the `attempt` combinator and return Ok when 200 *)
  let rec retry_til_ok (n : int) (url : string) : ((exn, int) result) io =
    if n = 0 then IO.pure @@ Result.Error "Could not get 200"
    else IO.(attempt (status_of_url url) >>= (
      function
        | Result.Ok 200 as ok -> IO.pure ok
        | _ -> retry_til_ok (n - 1) url
    ))
    
```
