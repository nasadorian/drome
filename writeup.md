Narek Asadorian \
CSCI E-51 \
Spring 2021

---

# drome: The IO Monad and Friends for OCaml

![](img/camel-train.jpg)

## Abstract

OCaml sits at a "sweet spot" in the world of programming languages as it provides support for multiple programming paradigms, a powerful type system and novel abstractive capabilities. It is a friendly functional language well suited for beginners, but makes a tradeoff to achieve this status - it is neither pure nor lazy by default like its more advanced sister language Haskell. In an attempt to bridge this gap, we offer an implementation of the IO monad and related effect management utilities for OCaml.

Jane Street has previously addressed the effect system problem for OCaml by means of its algebraic effects library (White et. al. 2018, https://www.janestreet.com/tech-talks/effective-programming/). The OCaml ecosystem however seems to be missing a monadic option for effects, possibly due to the lack of higher kinded type abstraction in the language. Despite this limitation, we seek to demonstrate that the monadic approach to effect management can be implemented in a sane and user-friendly manner in OCaml. Given the presence of `option`, `result` and `either` monads (and their associated combinators) in the standard library, there is certainly room for an IO monad in the common functional programming style supported by OCaml. Further, we posit that monadic effects more naturally fit into the OCaml programming style than the callback-and-continuation algebraic style which also has a steeper, more mind-bending learning curve.

Our library is named `drome` after the versatile and hard-working Dromedary camel ubiquitous on the African content. It takes its inspiration and design cues from an effects system for Scala called `cats-effect` (Spiewak et. al., https://github.com/typelevel/cats-effect). At its core, `drome` is a domain specific language (DSL) for representing deferred IO computations embedded in OCaml as its host language. Indeed `drome`'s data types represent a lightweight programming language which can be compiled and executed within other OCaml programs synchronously or asynchronously by target interpreters found in the `IO` module.

## Getting Started

When developing `drome` programs we import a composite module aptly named `Drome` which combines the `IO`, `Resource` and `RefIO` modules under one namespace alongside the data types from our DSL. These are the only necessary modules for the end user to interact with. All of the demo code seen in this document is available in `demo.ml`.

## Typeclasses

Typeclasses represent small, interrelated pieces of functionality universally quantified over types and type constructors. They are are a central concept in strongly typed functional programming and it is thus assumed that the reader is at least partly familiar with `Functor`, `Applicative` and `Monad`.

The `IO` and `Resource` datatypes present in `drome` admit a subset of the typeclass hierarchy found in Haskell's `Prelude` or in Scala's `cats` library. There are also two extensions `ApplicativeError` and `MonadError` specifically useful for the `IO` type which support  error handling capabilities. All typeclass instances live in the `instances.ml` module. When importing any of `drome`'s modules, the respective typeclass instances and special syntax are automatically included, so the user does not need to explicitly import typeclass instances.

Given the hierarchical nature of typeclasses, we are able to derive
`Applicative` and `Functor` given a `Monad` instance so a "module-functor" is
used to that end. The `ApplicativeError` class is implemented manually but the `MonadError`
class is derived through a module-functor.

As a caveat, the end user should carefully scope their module imports due to the
shared operator names between each typeclass instance. For example when using both `IO`
and `Resource` together in a program, it is suggested to use module scoping
syntax such as `IO.(...)` in place of `open IO` at the top of a file.
This will prevent compile errors confusing duplicate operators such as `Resource.(>>=)` and `IO.(>>=)`. 

## The IO Monad

So what is the `IO` monad exactly besides a way to defer computations? `IO` is defined as a generalized algebraic datatype (GADT) encoding the means by which we can construct, compose and modify side-effecting programs. The various functions and combinators used in the `IO` API build up a sequence of operations as a pure program. It is important to note that given the inherent laziness of `IO`, programs are _descriptions_ of actions to be run at a later time just like code in a programming language. The DSL for `IO` is detailed below.

```
type _ io =
  | Pure : 'a -> 'a io
  | Suspend : 'a thunk -> 'a io
  | Bind : (('a -> 'b io) * 'a io) -> 'b io
  | Map : (('a -> 'b) * 'a io) -> 'b io
  | RaiseError : exn -> 'a io
  | Attempt : 'a io -> ('a, exn) result io
  | HandleErrorWith : ((exn -> 'a io) * 'a io) -> 'a io
```

* The simplest constructor `Pure` lifts a pure value into the `IO` context, without any deferral.
* `Suspend` captures a `thunk` (aka `unit -> 'a`), representing an IO action resulting in an `'a`. It is the main constructor and is instantiated via `IO.make`.
* `Map` encodes the typical `map` functionality, applying a function `f : 'a -> 'b` to an `'a io` and resulting in a `'b io`. 
* `Bind` encodes `bind` or "flatMap", representing the chaining of `IO` programs together through a function `'a -> 'b io`. We will see that `Bind` is the most commonly used and strictly most powerful node in the entire DSL. In fact, `Bind` can be used to represent `Map`, but we maintain `Map` as separate due to specific optimizations we can perform on it.
* `RaiseError` sequences an error to be thrown in the running of the program.
* `Attempt` captures the first error arising from the program beneath it, yielding a `result` datatype with the error in the right channel.
* `HandleErrorWith` captures errors discards them and provides a default value of type `'a io`.

We'll begin our tour of the library with the canonical "hello world" example using `IO`. The main entry point for `IO` programs is the `IO.make` function which lifts a deferred computation into `IO`. _Note: Results from executed programs are shown as comments below their respective code snippets._

```ocaml
open Drome

let hello = IO.make (fun _ -> print_endline "Hello world!");;

IO.unsafe_run_sync hello;;
(*
  Hello world!
  - : unit = ()
*)
```

Next, we want to show that `IO` programs can be composed together arbitrarily and even recursively. It should be noted that the possibility for infinite lazy recursion is not a small feat in `drome`, and is achieved using a technique known as "trampolining" (https://en.wikipedia.org/wiki/Trampoline_(computing)).

To motivate this concept, imagine if one were to implement a simple looping type naively as `type 'a loop = Loop of ('a loop)` and attempt to recursively write the below function `go`. The OCaml runtime would crash when evaluating `go` because it creates an eagerly evaluated infinite data structure and causes a stack overflow.

```ocaml
type 'a loop = Loop of ('a loop);;

let rec go a = Loop (go a);;

go 1;;
(*
  Stack overflow during evaluation (looping recursion?).
*)
```

This problem is solved by introducing trampolining, specifically by modifying our `loop` type to contain a `thunk` evaluating to another `loop`. By deferring the next `loop` node, we create a _lazy_ infinitely nested data structure!

```ocaml
(* thunk is defined in Util as the alias type 'a thunk = unit -> 'a *)
open Util;;

type 'a loop = Loop of ('a loop thunk);;

let rec go a = Loop (fun _ -> go a);;

go 1;;
(*
  - : 'a loop = Loop <fun>
*)
```

We can apply this same technique to the `IO` data type, and enable recursive binds between actions. In the example below, we use `IO.make` and the `IO.(>>=) aka bind` operator to create an infinitely looping read-and-print program.

```ocaml
  let read : string io = IO.make read_line
  let print (s : string) : unit io = IO.make (fun _ -> print_endline s)
  
  (* Combine read and print infinitely via trampolined bind *)
  let rec echo () : unit io = IO.( read >>= print >>= echo ) 

  (* Run synchronously *)
  IO.unsafe_run_sync echo;;
  (*
    <in>hi
    hi
    <in>there
    there
    ^CInterrupted.
  *)
```

To aid in interpreting the `echo` function, let's break down what it does. We use the `read` function to lazily prompt the user at `stdin` then bind the resulting string into another action `print` which simply echoes the string back to `stdout`. The result of the print has type `unit io`, and since our `echo` function takes a unit argument, we bind into `echo` again. When desugared, the `echo` program infinitely expands outward as `Bind (fun _ -> Bind (fun s -> Suspend (fun _ -> print_endline s), Suspend (fun _ -> read_line)), ...)` but can be evaluated node by node thanks to the trampoline.

There is another interesting technique at work under the hood in this example. When the interpreter reaches a node containing `Bind (f, Bind (g, rest))` it rearranges the two constructors using the associativity law for Monad (https://wiki.haskell.org/Monad_laws) to yield `Bind (g >=> f, rest)`. The `IO.(>=>)` operator is known as "the fish" or Kleisli composition, and it chains two monadic functions together. In cases where there are infinitely nested binds, this property allows the interpreter to make a tail recursive call to itself and make progress rather than infinitely build up the call stack. See the implementation of `IO.unsafe_run_sync` for more.

_Monad associativity as it translates to the IO DSL_.

```
Bind (f, Bind (g, io))  =  (io >>= g) >>= f
Bind (g >=> f, io)      =  io >>= (fun x -> (g x) >>= f)
```

Of course, far more interesting programs than `echo` can be written with `IO`. Take for example the below set of functions which query a website until it yields an `HTTP/200 OK` response. The `url` value is an `IO` program randomly returning one of two test URLs that result in a `200` or `404`, modeling a flaky service we would like to health-check. The `status_of_url` function sends a `GET` request to a given URL and returns its status code as an integer. In `retry_til_ok` we run `status_of_url` up to `n` times, utilizing the `IO.attempt` combinator to capture any runtime errors in a `result` type, and match on the response code. We use the recursive bind trick here to retry this function until a `200` response is received.

```ocaml
  open Cohttp
  open Cohttp_lwt_unix
  open Drome

  (* Return one of two URLs based on a coin toss *)
  let url : Uri.t io =
    IO.make (fun _ ->
        (if Random.int 2 = 0 then "https://httpstat.us/200"
        else "https://httpstat.us/404")
        |> Uri.of_string)

  (* GET a URL and convert its status to an integer *)
  let status_of_url (url : Uri.t) : int io =
    IO.make (fun _ ->
        Client.get url |> Lwt_main.run |> fst |> Response.status
        |> Code.code_of_status)

  (* Handle errors using the `attempt` combinator and return Ok when 200 *)
  let rec retry_til_ok (n : int) : (int, exn) result io =
    let open IO in
    if n = 0 then pure @@ Result.error (Failure "unable to reach test URL")
    else
      url >>= fun u ->
      status_of_url u |> attempt >>= function
      | Result.Ok 200 -> pure (Result.ok 200)
      | _ ->
          print_endline "Failed, retrying";
          retry_til_ok (n - 1)

  retry_til_ok 10 |> IO.unsafe_run_sync;;
  (*
    Failed, retrying
    Failed, retrying
    - : (int, exn) result = Ok 200
  *)
```

## Resource

The side-effecting actions we use are not always stateless. In many cases, we would like to perform effects on resources which require acquisition and cleanup steps. Enter `Resource`, a utility which builds on top of `IO` to support exactly this pattern. To construct a closeable resource we use the `Resource.make` function which accepts two arguments: `acquire` which is the action producing the resource of type `'a io`, and `release` which is a function `'a -> unit io` closing the resource. The `release` action will _always run_ even if intermediate steps fail when using the resource, not unlike the "context manager" construct in Python or Java's `try-catch-finally`. Errors thrown during the usage of a resource will be sequenced _after_ the resource has been finalized.

With `Resource` we can open a file handle, pipe all of its contents into a memory location then finally close the handle. In the below code snippet, we construct a `Resource` using our `open_file` and `close` functions. We can then execute an `IO` action on this self-closing file handle by calling `Resource.use` with `drain_file`. Our `close` function will, upon finalization, write a sentinel value to the memory location as well to prove that it ran. When we inspect the contents of the memory location after the file has been used, we see that the string "closed" is the last on the stack.

```ocaml
  (* open a file in IO context *)
  let open_file path = IO.make (fun _ -> open_in path)

  (* close file and write watermark `s` into memory location *)
  let close s mem c =
    IO.make (fun _ ->
        close_in c;
        (* add a watermark to prove finalizer was executed *)
        mem := s :: !mem)

  (* drain a file line by line into memory location *)
  let drain_file mem file =
    IO.make (fun _ ->
        try
          while true do
            mem := input_line file :: !mem
          done
        with End_of_file -> ())

  (* open file, write all contents to memory, close file, print contents *)
  let go _ =
    let mem = ref [] in
    let handle =
      Resource.make (open_file "test-data/file0") (close "closed" mem)
    in
    let _ = Resource.use (drain_file mem) handle |> IO.unsafe_run_sync in
    List.iter print_endline !mem

  go ();;
  (*
    closed
    file0-row3
    file0-row2
    file0-row1
    file0-row0
    - : unit = ()
  *)
```

`Resource` is implemented as a typeclass-obedient datatype with its own
constructors. Note here that there is no `RMap` - we rely on `RBind` to
implement mapping behavior. The algebra for `Resource` compiles down to an `IO`
program when the `Resource.use` interpreter is called.

```ocaml
type _ resource =
  | Allocate : ('a io * ('a -> unit io)) -> 'a resource
  | RPure : 'a -> 'a resource
  | RBind : (('a -> 'b resource) * 'a resource) -> 'b resource
```

In order to demonstrate some of `Resource`'s typeclass capabilities, we reach
for 3 different file handles and zip them together using the `Applicative.( >*<
) aka zip` operation. We allow the `drain` computation to fail as soon as one file has been exhausted, without a try-catch block. While allowing inexplicit exceptions to be thrown is not a suggested usage pattern, this example proves that that resources will be closed regardless of errors. See below how all three resource finalizers run in reverse order, and the arising error is maintained and resequenced.

```ocaml
  (* open a file in IO context *)
  let open_file path = IO.make (fun _ -> open_in path)

  (* close file *)
  let close s f =
    IO.make (fun _ ->
        print_endline ("closing " ^ s);
        close_in f)

  let file0, file1, file2 =
    Resource.
      ( make (open_file "./test-data/file0") (close "file0"),
        make (open_file "./test-data/file1") (close "file1"),
        make (open_file "./test-data/file2") (close "file2") )

  (* zip resources together, Applicative style *)
  let zipped = Resource.(file0 >*< file1 >*< file2)

  (* print file contents as triples *)
  let drain ((f0, f1), f2) : unit io =
    IO.make (fun _ ->
          while true do
            let x, y, z = (input_line f0, input_line f1, input_line f2) in
            print_endline (Printf.sprintf "%s, %s, %s" x y z)
          done
        )

  (* execute the program *)
  let go _ = Resource.use drain zipped |> IO.unsafe_run_sync

  go ();;
  (*
    file0-row0, file1-row0, file2-row0
    file0-row1, file1-row1, file2-row1
    file0-row2, file1-row2, file2-row2
    file0-row3, file1-row3, file2-row3
    closing file2
    closing file1
    closing file0
    Exception: End_of_file
    - : unit = ()
  *)
```

## RefIO

There is another useful datatype we can implement on top of `IO` called
`RefIO`, in the literature simply referred to as `Ref` but renamed here to avoid confusion
with OCaml's `ref` primitive. The `RefIO` type represents a purely
functional reference that is thread-safe and supports atomic access without any
locking primitives. `RefIO`'s API is detailed below.

```ocaml
module type RefIO_API = sig
  type 'a f
  val make : 'a -> 'a f io
  val set : 'a -> 'a f -> unit io
  val get : 'a f -> 'a io
  val update : ('a -> 'a) -> 'a f -> unit io
  val modify : ('a -> 'a * 'b) -> 'a f -> 'b io
end
```

Every function modifying a purely functional reference is suspended in `IO`,
and it is indeed the laziness of the `IO` type which allows atomic updates. Since any chain of actions performed on a `RefIO` (`get`, `set`, etc.) results in an `IO`, we declaratively build up a sequence of deferred actions to run _in order_ when it comes time to execute.

Beyond atomicity in `get` and `set`, `RefIO` provides the `update` and `modify` functions which perform "get-then-set" actions at once with the underlying reference.
This style of behavior is enabled by a simple concurrency trick called a "compare-and-swap loop" (https://en.wikipedia.org/wiki/Compare-and-swap). When performing an update or modification to the reference, an inner loop function will continually attempt to verify that the underlying reference has not changed since call time before it makes any changes.  

In the example below we bring together multiple concepts from `drome`, using
threads to asynchronously update a shared mutable database. We
introduce a `student` datatype and a mutable database containing the students
in a school.

The `database` value contains a reference to the initial state of the database, and a number of functions are composed to perform the `update_database` routine which processes a list of `action` objects against the database concurrently. Another routine called `run_tardy_report` counts the number of tardy students for the day and resets all tardy fields to `false`.

The updates occuring within `update_database` run in multiple OCaml threads via the `IO.suspend_async'` function. It is also worthwhile to note that the top level reference to `database` is pure since it is deferred and can only be mutated by direct reference to one instance of it. So without passing the same reference to both the `update_database` and `run_tardy_report` functions we will not be updating the same instance of the database. In the last example, we show that passing the same reference to an expression which runs both functions together will achieve successive transformations.

```ocaml
  (* student rows for database *)
  type student = { name : string; mutable age : int; mutable tardy : bool }

  (* actions observed during the school day *)
  type action =
    | RollCall of (string * bool)
    | Birthday of (string * int)
    | NewStudent of student

  (* a mutable database of students *)
  let database =
    RefIO.make
      [
        { name = "Alice"; age = 13; tardy = false };
        { name = "Bob"; age = 12; tardy = true };
        { name = "Carol"; age = 13; tardy = false };
      ]

  let find_student (name : string) : student list -> student option =
    List.find_opt (fun s -> s.name = name)

  let update_tardy (tardy : bool) (s : student) : unit = s.tardy <- tardy

  let update_age (age : int) (s : student) : unit = s.age <- age

  (* execute all actions against the database *)
  let run_actions (al : action list) (sl : student list) : student list =
    List.fold_left
      (fun db a ->
        match a with
        | RollCall (stu, b) ->
            let _ = Option.map (update_tardy b) (find_student stu db) in
            db
        | Birthday (stu, age) ->
            let _ = Option.map (update_age age) (find_student stu db) in
            db
        | NewStudent s -> s :: db)
      sl al

  (* dump database records to stdout *)
  let print_database (db : student list RefIO.f) : unit io =
    IO.(
      RefIO.get db >>= fun l ->
      make (fun _ ->
          List.iter
            (fun { name = n; age = a; tardy = t } ->
              print_endline
                (Printf.sprintf "{ name=%s; age=%i; tardy=%b }" n a t))
            l))

  (* tabulate tardy count and reset tardies for the next day *)
  let tardy_report : student list RefIO.f -> int io =
    RefIO.modify (fun l ->
        let count = List.(filter (fun s -> s.tardy) l |> length) in
        let _ = List.iter (fun s -> s.tardy <- false) l in
        (l, count))

  (* count and print tardies, printing the database afterward *)
  let run_tardy_report db =
    let open IO in
    tardy_report db >>= fun c ->
    make (fun _ -> print_endline (Printf.sprintf "%d students tardy today" c))
    *> print_database db

  (* school collects actions and updates the database daily, concurrently *)
  let update_database (l : action list) (db : student list RefIO.f) : unit io =
    let rolls, birthdays, news =
      List.fold_left
        (fun (r, b, n) a ->
          match a with
          | RollCall _ -> (a :: r, b, n)
          | Birthday _ -> (r, a :: b, n)
          | NewStudent _ -> (r, b, a :: n))
        ([], [], []) l
    in
    let p0 = RefIO.update (run_actions rolls) db |> IO.suspend_async' in
    let p1 = RefIO.update (run_actions birthdays) db |> IO.suspend_async' in
    let p2 = RefIO.update (run_actions news) db |> IO.suspend_async' in
    IO.(p0 *> p1 *> p2 *> sleep 0.1 *> print_database db)

    (* run updates against the database *)
    IO.(database >>= 
      (update_database [
        Birthday ("Bob", 42);
        RollCall ("Alice", false);
        NewStudent ({name="Derek";age=11;tardy=false});
        RollCall ("Derek", true)]) |> unsafe_run_sync);;
    (*
      { name=Derek; age=11; tardy=true }
      { name=Alice; age=13; tardy=false }
      { name=Bob; age=42; tardy=true }
      { name=Carol; age=13; tardy=false }
      - : unit = ()
    *)

    (* run tardy report, modifying the database -- purity! *)
    IO.(database >>= run_tardy_report |> unsafe_run_sync);;
    (*
      1 students tardy today
      { name=Alice; age=13; tardy=false }
      { name=Bob; age=42; tardy=false }
      { name=Carol; age=13; tardy=false }
      - : unit = ()
    *)

   (* run updates and tardy report on the same instance of the database *)
    IO.(
      let prog =
        database >>= fun db ->
        update_database
          [
            NewStudent { name = "Derek"; age = 13; tardy = false };
            RollCall ("Derek", true);
            RollCall ("Alice", true);
          ]
          db
        *> run_tardy_report db
      in
      unsafe_run_sync prog)
    (*
      { name=Derek; age=13; tardy=true }
      { name=Alice; age=13; tardy=true }
      { name=Bob; age=12; tardy=true }
      { name=Carol; age=13; tardy=false }
      3 students tardy today
      { name=Derek; age=13; tardy=false }
      { name=Alice; age=13; tardy=false }
      { name=Bob; age=12; tardy=false }
      { name=Carol; age=13; tardy=false }
      - : unit = ()
    *)
```

## Runtime Implementation

Finally we can briefly discuss the runtime implementations for `IO` programs
available in `drome`. The central runtime which serves as the basis for the others is the interpreter called `IO.unsafe_run_sync`. This function performs optimizations such as map fusion (https://en.wikipedia.org/wiki/Map_(higher-order_function)#Optimizations) and trampolined bind associativity while executing actions defined by the `IO` DSL. Optimizations occur by pattern matching a subsequent level when `Bind` or `Map` occurs in the program. As a result of these optimizations, this interpreter is tail-recursive and can thus be used on very large programs without stack overflow concerns.

In this document we've also made use of `IO.unsafe_run_async'` and `IO.suspend async'` which give the ability to run an `IO` program in another thread. The asynchronous components of the runtime rely on the main interpreter and the basic threading primitives found in OCaml. In the asynchronous runtimes we provide the option to run a callback on the results of the computation. Non-callback variants are tagged with a prime as in `IO.unsafe_run_async'`. It is acknowledged that in OCaml `4.11.x` there is no true concurrency model, so the threads being "spawned" are merely coroutines. However even with the advent of proper concurrency our approach would still work, pushing the synchronous interpreter into a new thread.

The full fledged power of a synchronous `IO` runtime is best appreciated when other "languages" are transpiled into `IO`. For example in the case of `Resource`, we create a new DSL with its own semantics which can through the interpretation become an `IO` program. Indeed this concept can be taken to further a extent for future features such as cancelable asynchrony or thread scheduling.

## Acknowledgements

* `drome` takes design inspiration from the `cats-effect` library for Scala
(https://typelevel.org/cats-effect/).
* `RefIO` implementation was informed by Fabio Labella's talk on Ref and Deffered (https://vimeo.com/366191463)
* The Free monad encoding of `IO` and some of the runtime optimizations seen in `drome` are also inspired by Runar Bjarnason and Paul Chiusano's "Functional Programming in Scala" (https://www.manning.com/books/functional-programming-in-scala)


