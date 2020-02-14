open Core
open Async
open Deribit_ws

module Encoding = Json_encoding.Make(Json_repr.Yojson)
let buf = Bi_outbuf.create 4096

let of_string s =
  Encoding.destruct encoding (Yojson.Safe.from_string ~buf s)
let to_string t =
  Yojson.Safe.to_string ~buf (Encoding.construct encoding t)

let rec inner = function
  | 0 -> Deferred.unit  | n when n > 0 ->
    Fastws_async.with_connection
      url_test ~of_string ~to_string begin fun _r _w ->
      Logs_async.app (fun m -> m "inner %d" n)
    end >>= fun () ->
    Clock_ns.after (Time_ns.Span.of_int_sec 3) >>= fun () ->
    inner (pred n)
  | _ -> invalid_arg "inner"

let () =
  Command.async ~summary:"Deribit leak test" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param []
      and n = anon ("n" %: int) in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        inner n
    ] end |>
  Command.run
