open CS51Utils.Absbook
open Drome
open IO

let open_file path = suspend (fun _ -> open_in path)

let gen_file c =
  let next () = input_line c in
  next

let resource_tests _ =
  let mem = ref [] in
  let file = open_file "test-data/file0" in
  let close c =
    suspend (fun _ ->
        close_in c;
        mem := "closed" :: !mem)
  in
  let rec read c =
    suspend (fun _ ->
        let s = input_line c in
        mem := s :: !mem;
        c)
    >>= read
  in
  let res = Resource.make file close in
  let out = Resource.use read res |> attempt |> unsafe_run_sync in

  unit_test
    (!mem = [ "closed"; "file0-row3"; "file0-row2"; "file0-row1"; "file0-row0" ]
    && out = Result.error End_of_file)
    "Resource.use -- drain a file and close it";
  let mem = ref [] in
  let close c =
    suspend (fun _ ->
        close_in c;
        mem := ("closed", "closed", "closed") :: !mem)
  in
  let file0, file1, file2 =
    Resource.
      ( make (open_file "./test-data/file0") close,
        make (open_file "./test-data/file1") close,
        make (open_file "./test-data/file2") close )
  in
  let zipped = Resource.(file0 >*< file1 >*< file2) in
  let prog =
    Resource.use
      (fun ((f0, f1), f2) ->
        suspend (fun _ ->
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
         ("closed", "closed", "closed");
         ("closed", "closed", "closed");
         ("closed", "closed", "closed");
         ("file0-row3", "file1-row3", "file2-row3");
         ("file0-row2", "file1-row2", "file2-row2");
         ("file0-row1", "file1-row1", "file2-row1");
         ("file0-row0", "file1-row0", "file2-row0");
       ]
    && out = Result.ok ())
    "Resource.zip -- join 3 files"

let _ = resource_tests ()
