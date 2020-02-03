open Core
open Async
open Deribit_rest
open Fixtypes

let wrap_request
    ?(timeout=Time.Span.of_int_sec 5)
    ?(speed=`Quick) n service =
  Alcotest_async.test_case ~timeout n speed begin fun () ->
    (Fastrest.request service) |>
    Deferred.ignore_m
  end

let wrap_request_light
    ?(timeout=Time.Span.of_int_sec 5)
    ?(speed=`Quick) n f =
  Alcotest_async.test_case ~timeout n speed begin fun () ->
    f () |>
    Deferred.ignore_m
  end

let key, secret =
  match String.split ~on:':' (Sys.getenv_exn "TOKEN_DERIBIT_TEST") with
  | [key; secret] -> key, secret
  | _ -> assert false

let date =
  Option.value_exn (Ptime.of_date_time ((2019,08,23), ((0,0,0),0)))

let raise_on_error f = Deferred.ignore_m (f ())

let rest = [
  wrap_request "futures.BTC" (Instrument.get ~kind:SecurityType.Future "BTC" url) ;
  wrap_request "futures.ETH" (Instrument.get ~kind:SecurityType.Future "ETH" url) ;
  wrap_request "options.BTC" (Instrument.get ~kind:SecurityType.Option "BTC" url) ;
  wrap_request "options.BTC" (Instrument.get ~kind:SecurityType.Option "ETH" url) ;
  wrap_request "all.BTC" (Instrument.get "BTC" url) ;
]

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Info) ;
  Alcotest.run "deribit" [
    "rest", rest ;
  ]
