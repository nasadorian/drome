open Cohttp
open Cohttp_lwt_unix
open Dsl
open Drome

module HTTP_Demo = struct
  let url : Uri.t io =
    IO.make (fun _ ->
        (if Random.int 1 = 0 then "https://httpstat.us/200"
        else "https://httpstat.us/404")
        |> Uri.of_string)

  (*GET a URL and convert its status to an integer *)
  let status_of_url (url : Uri.t) : int io =
    IO.make (fun _ ->
        Client.get url |> Lwt_main.run |> fst |> Response.status
        |> Code.code_of_status)

  (*Handle errors using the `attempt` combinator and return Ok when 200 *)
  let rec retry_til_ok (n : int) : (int, exn) result io =
    let open IO in
    url >>= fun u ->
    status_of_url u |> attempt >>= function
    | Result.Ok 200 -> pure (Result.ok 200)
    | _ -> retry_til_ok (n - 1)
end
