open Cohttp
open Cohttp_lwt_unix
open Drome

module Echo_Demo = struct
  (*
    Understanding trampolined infinite binds...

    read = Suspend (fun _ -> read_line)
    print = fun s -> Suspend (fun _ -> print_endline s)
    echo = fun _ -> read >>= print >>= echo

    - This is `echo` without unfolding the recursion
    Bind (fun _ -> echo, Bind (print, read))

    - After bind associativity is applied
    Bind (print >=> (fun _ -> echo), read)

    - Substitute `read` function for its constructor
    - Now the interpreter can run this node
    Bind (print >=> (fun _ -> echo), Suspend (fun _ -> read_line))
  *)

  (* infinite looping test -- interactive and can be tested manually *)
  let read = IO.make read_line

  let print (s : string) : unit io = IO.make (fun _ -> print_endline s)

  (* trampolining used in IO.unsafe_run_async allows infinite loops! *)
  let rec echo () = IO.(read >>= print >>= echo)
end

module HTTP_Demo = struct
  (* return one of two URLs based on a coin toss *)
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

  (* handle errors using the `attempt` combinator and return Ok when 200 *)
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
end

module Resource_File = struct
  (* open a file in IO context *)
  let open_file path = IO.make (fun _ -> open_in path)

  (* close file and write matermark `s` into memory location *)
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
end

module Resource_Zip = struct
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
        done)

  (* execute the program *)
  let go _ = Resource.use drain zipped |> IO.unsafe_run_sync
end

module Database = struct
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
  let run_tardy_report (db : student list RefIO.f) : unit io =
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
end
