open Core
open Async

open Deribit_ws

let src = Logs.Src.create "deribit.ws-test"  ~doc:"Deribit API - WS test application"
module Log = (val Logs.src_log src : Logs.LOG)
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

let get_id =
  let id = ref 0 in
  fun () -> let ret = !id in incr id ; ret

let process_user_cmd w =
  let process s =
    match String.split s ~on:' ' with
    | "trades" :: syms ->
      let syms = List.map syms ~f:(fun s -> trade_chan s) in
      Pipe.write w (Subscribe { id = get_id () ; body = syms})
    | "perp" :: syms ->
      let syms = List.map syms ~f:(fun s -> perp_chan s) in
      Pipe.write w (Subscribe { id = get_id () ; body = syms})
    | "index" :: syms ->
      let syms = List.map syms ~f:(fun s -> index_chan s) in
      Pipe.write w (Subscribe { id = get_id () ; body = syms})
    | "ranking" :: syms ->
      let syms = List.map syms ~f:(fun s -> ranking_chan s) in
      Pipe.write w (Subscribe { id = get_id () ; body = syms})
    | "books" :: syms ->
      let syms = List.map syms ~f:(fun s -> book_chan s) in
      Pipe.write w (Subscribe { id = get_id () ; body = syms})
    | "all" :: syms ->
      let syms = List.(map syms ~f:(fun s -> [trade_chan s; book_chan s]) |> concat) in
      Pipe.write w (Subscribe { id = get_id () ; body = syms})
    | "untrades" :: syms ->
      let syms = List.map syms ~f:(fun s -> trade_chan s) in
      Pipe.write w (Unsubscribe { id = get_id () ; body = syms})
    | "unbooks" :: syms ->
      let syms = List.map syms ~f:(fun s -> book_chan s) in
      Pipe.write w (Unsubscribe { id = get_id () ; body = syms})
    | "unsubscribe" :: syms ->
      let syms = List.(map syms ~f:(fun s -> [trade_chan s; book_chan s]) |> concat) in
      Pipe.write w (Unsubscribe { id = get_id () ; body = syms})
    (* | "ping" :: v :: _ ->
     *   Pipe.write w (Ping (int_of_string_opt v))
     * | "ping" :: _ ->
     *   Pipe.write w (Ping None) *)
    | h :: _ ->
      Log_async.err (fun m -> m "Unknown command %s" h)
    | [] ->
      Log_async.err (fun m -> m "Empty command")
  in
  let rec loop () = Reader.(read_line @@ Lazy.force stdin) >>= function
    | `Eof -> Deferred.unit
    | `Ok line -> process line >>= loop
  in
  loop ()

let main () =
  let module Encoding = Json_encoding.Make(Json_repr.Yojson) in
  let buf = Bi_outbuf.create 4096 in
  let of_string s =
    Encoding.destruct encoding (Yojson.Safe.from_string ~buf s) in
  let to_string t =
    Yojson.Safe.to_string ~buf (Encoding.construct encoding t) in
  Fastws_async.with_connection ~of_string ~to_string url begin fun r w ->
    let log_incoming msg = Log_async.debug (fun m -> m "%a" pp msg) in
    Deferred.all_unit [
      process_user_cmd w ;
      Pipe.iter r ~f:log_incoming
    ]
  end

let () =
  Command.async ~summary:"Deribit WS client" begin
    let open Command.Let_syntax in
    [%map_open
      let () = Logs_async_reporter.set_level_via_param [] in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ] end |>
  Command.run
