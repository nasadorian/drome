(*
  resource_tests.ml -- unit tests for the Resource module
*)

open CS51Utils.Absbook
open Drome
open IO

(* open a file in IO context *)
let open_file path = make (fun _ -> open_in path)

(* close file and write matermark `s` into memory location *)
let close s mem c =
  make (fun _ ->
      close_in c;
      (* add a watermark to prove finalizer was executed *)
      mem := s :: !mem)

(* drain a file line by line into memory location *)
let drain_file mem file =
  Resource.(
    file <$> fun c ->
    try
      while true do
        mem := input_line c :: !mem
      done
    with End_of_file -> ())

(* print contents of file to console; useful for debugging *)
let print_file : in_channel Dsl.resource -> unit Dsl.io =
  Resource.use (fun c ->
      IO.make (fun _ ->
          try
            while true do
              input_line c |> print_endline
            done
          with End_of_file -> ()))

(* check basic Resource mechanics, assuring unconditional finalize step *)
let basic _ =
  let mem = ref [] in
  let file = open_file "test-data/file0" in
  (* infinitely loop on file allowing End_of_file to be thrown *)
  let rec read c =
    make (fun _ ->
        let s = input_line c in
        mem := s :: !mem;
        c)
    >>= read
  in
  let res = Resource.make file (close "closed" mem) in
  let out = Resource.use read res |> attempt |> unsafe_run_sync in
  unit_test
    (!mem = [ "closed"; "file0-row3"; "file0-row2"; "file0-row1"; "file0-row0" ]
    && out = Result.error End_of_file)
    "Resource.use -- drain a file and close it"

(* zip together 3 Resources and ensure they close in reverse order post-use *)
let zip_3_files _ =
  let mem = ref [] in
  let file0, file1, file2 =
    Resource.
      ( make
          (open_file "./test-data/file0")
          (close ("file0-closed", "file0-closed", "file0-closed") mem),
        make
          (open_file "./test-data/file1")
          (close ("file1-closed", "file1-closed", "file1-closed") mem),
        make
          (open_file "./test-data/file2")
          (close ("file2-closed", "file2-closed", "file2-closed") mem) )
  in
  let zipped = Resource.(file0 >*< file1 >*< file2) in
  let prog =
    Resource.use
      (fun ((f0, f1), f2) ->
        make (fun _ ->
            try
              while true do
                mem := (input_line f0, input_line f1, input_line f2) :: !mem
              done
            with End_of_file -> ()))
      zipped
  in
  let out = prog |> attempt |> unsafe_run_sync in
  unit_test
    (!mem
     = [
         ("file0-closed", "file0-closed", "file0-closed");
         ("file1-closed", "file1-closed", "file1-closed");
         ("file2-closed", "file2-closed", "file2-closed");
         ("file0-row3", "file1-row3", "file2-row3");
         ("file0-row2", "file1-row2", "file2-row2");
         ("file0-row1", "file1-row1", "file2-row1");
         ("file0-row0", "file1-row0", "file2-row0");
       ]
    && out = Result.ok ())
    "Resource.zip -- join 3 files"

(* map file into generator and drain it *)
let map_drain _ =
  let mem = ref [] in
  let file = Resource.make (open_file "test-data/file2") (close "closed" mem) in
  let gen = drain_file mem file in
  let _ = Resource.use' gen |> IO.unsafe_run_sync in
  unit_test
    (!mem = [ "closed"; "file2-row3"; "file2-row2"; "file2-row1"; "file2-row0" ])
    "Resource.map -- drain a file generator"

(* bind resources into another resource *)
let bind_drain _ =
  let mem = ref [] in
  let name0 = Resource.return "test-data" in
  let name1 = Resource.(name0 >>= fun n -> return (n ^ "/file1")) in
  let prog =
    Resource.(
      name1
      >>= (fun p -> Resource.make (open_file p) (close "closed" mem))
      |> drain_file mem)
  in
  let _ = Resource.use' prog |> IO.unsafe_run_sync in
  unit_test
    (!mem = [ "closed"; "file1-row3"; "file1-row2"; "file1-row1"; "file1-row0" ])
    "Resource.bind -- multiple binds"

(* run all of the tests! *)
let _ =
  basic ();
  zip_3_files ();
  map_drain ();
  bind_drain ()
