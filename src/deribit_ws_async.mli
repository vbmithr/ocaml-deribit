open Async

open Deribit_ws

val connect : unit ->
  (t Pipe.Reader.t * t Pipe.Writer.t * unit Deferred.t) Deferred.Or_error.t

val connect_exn : unit ->
  (t Pipe.Reader.t * t Pipe.Writer.t * unit Deferred.t)
    Deferred.t

val with_connection :
  (t Pipe.Reader.t -> t Pipe.Writer.t -> 'a Deferred.t) ->
  'a Deferred.Or_error.t

val with_connection_exn :
  (t Pipe.Reader.t -> t Pipe.Writer.t -> 'a Deferred.t) ->
  'a Deferred.t
