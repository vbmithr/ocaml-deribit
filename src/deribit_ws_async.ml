open Core
open Async
open Deribit_ws

let src = Logs.Src.create "deribit.ws.async"
module Log = (val Logs.src_log src : Logs.LOG)

module T = struct
  type t = {
    r: Deribit_ws.t Pipe.Reader.t ;
    w: Deribit_ws.t Pipe.Writer.t ;
  }

  let create r w = { r; w }

  module Address = Uri_sexp

  let is_closed { r; w } = Pipe.(is_closed r && is_closed w)
  let close { r; w } =
    Pipe.close w ;
    Pipe.close_read r ;
    Deferred.unit
  let close_finished { r; w } =
    Deferred.all_unit [Pipe.closed r;
                       Pipe.closed w]
end
include T

let create_client_read =
  Pipe.map ~f:begin fun msg ->
    Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
  end

let create_client_write w =
  Pipe.create_writer begin fun ws_read ->
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      let doc =
        match Ezjsonm_encoding.construct encoding cmd with
        | `A _ | `O _ as a -> Ezjsonm.to_string a
        | _ -> invalid_arg "not a json document" in
      Log.debug (fun m -> m "-> %s" doc) ;
      doc
    end
  end

let connect url =
  Deferred.Or_error.map
    (Fastws_async.connect url) ~f:begin fun { r; w; _ } ->
    let client_write = create_client_write w in
    (Pipe.closed client_write >>> fun () -> Pipe.close w) ;
    create (create_client_read r) client_write
  end

module Persistent = struct
  include Persistent_connection_kernel.Make(T)

  let create' ~server_name ?on_event ?retry_delay =
    create ~server_name ?on_event ?retry_delay ~connect
end

let connect_exn url =
  connect url >>= function
  | Error e -> Error.raise e
  | Ok a -> return a

let with_connection url f =
  Fastws_async.with_connection url ~f:begin fun r w ->
    f (create_client_read r) (create_client_write w)
  end

let with_connection_exn url f =
  with_connection url f >>= function
  | Error e -> Error.raise e
  | Ok a -> return a
