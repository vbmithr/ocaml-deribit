open Sexplib.Std
open Json_encoding

module Ezjsonm_encoding = struct
  include Json_encoding.Make(Json_repr.Ezjsonm)

  let destruct_safe encoding value =
    try destruct encoding value with exn ->
      Format.eprintf "%a@."
        (Json_encoding.print_error ?print_unknown:None) exn ;
      raise exn
end

module Ptime = struct
  include Ptime

  type dps = int * int64 [@@deriving sexp]

  let span_of_sexp sexp =
    Ptime.Span.unsafe_of_d_ps (dps_of_sexp sexp)

  let sexp_of_span span =
    sexp_of_dps (Ptime.Span.to_d_ps span)

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 ~frac_s:7 t)

  let ms_encoding =
    let open Json_encoding in
    conv
      (fun t -> 1e3 *. Ptime.to_float_s t)
      (fun ts -> match Ptime.of_float_s (ts /. 1e3) with
         | None -> invalid_arg "Ptime.encoding"
         | Some ts -> ts)
      (ranged_float ~minimum:1e12 ~maximum:max_float "us_encoding")

  let us_encoding =
    let open Json_encoding in
    conv
      (fun t -> 1e6 *. Ptime.to_float_s t)
      (fun ts -> match Ptime.of_float_s (ts /. 1e6) with
         | None -> invalid_arg "Ptime.encoding"
         | Some ts -> ts)
      (ranged_float ~minimum:1e15 ~maximum:max_float "us_encoding")

  let encoding =
    let open Json_encoding in
    union [
      case us_encoding (fun a -> Some a) (fun a -> a) ;
      case ms_encoding (fun a -> Some a) (fun a -> a) ;
    ]

  let us_span_encoding =
    conv
      (fun t -> Ptime.Span.to_float_s t *. 1e6)
      (fun d -> match Ptime.Span.of_float_s (d /. 1e6) with
         | Some span -> span
         | None -> invalid_arg "us_span_encoding")
      float
end

type channel =
  | Trades of string
  | Book of string
[@@deriving sexp]

let trade_chan sym = Trades sym
let book_chan sym = Book sym

let channel_of_string s =
  match String.split_on_char '.' s with
  | ["trades"; instr; "raw"] -> Some (Trades instr)
  | ["book"; instr; "raw"] -> Some (Book instr)
  | _ -> None

let channel_of_string_exn s =
  match channel_of_string s with
  | Some c -> c
  | None -> invalid_arg "channel_of_string"

let channel_encoding =
  conv
    (function
      | Trades instr -> "trades." ^ instr ^ ".raw"
      | Book instr -> "book." ^ instr ^ ".raw")
    channel_of_string_exn string

type 'a request = {
  id: int;
  body: 'a ;
} [@@deriving sexp]

let request_encoding meth e =
  conv
    (function { id ; body } -> ((), id, (), body))
    (fun ((), id, (), body) -> { id ; body })
    (obj4
       (req "jsonrpc" (constant "2.0"))
       (req "id" int)
       (req "method" (constant meth))
       (req "params" e))

let pub_subscribe_encoding =
  request_encoding "public/subscribe"
    (obj1 (req "channels" (list channel_encoding)))

let pub_unsubscribe_encoding =
  request_encoding "public/unsubscribe"
    (obj1 (req "channels" (list channel_encoding)))

let response_encoding =
  conv
    (fun (a, b, c, d, e) -> (), a, b, c, d, e)
    (fun ((), a, b, c, d, e) -> a, b, c, d, e)
    (obj6
       (req "jsonrpc" (constant "2.0"))
       (req "id" int)
       (req "testnet" bool)
       (req "usIn" Ptime.encoding)
       (req "usOut" Ptime.encoding)
       (req "usDiff" Ptime.us_span_encoding))

type msg = {
  code: int ;
  msg: string
} [@@deriving sexp]

let msg_encoding =
  let open Json_encoding in
  conv
    (fun { code ; msg } -> ( code, msg))
    (fun (code, msg) -> { code ; msg })
    (obj2
       (req "code" int)
       (req "message" string))

type ('a, 'b) result = ('a, 'b) Result.result =
    Ok of 'a | Error of 'b [@@deriving sexp]

type 'a response = {
  id: int ;
  testnet: bool ;
  result: ('a, msg) result ;
  usIn: Ptime.t ;
  usOut: Ptime.t ;
  usDiff: Ptime.span ;
} [@@deriving sexp]

let response_encoding e =
  conv
    (fun { id ; testnet ; result ; usIn ; usOut ; usDiff } ->
       result, (id, testnet, usIn, usOut, usDiff))
    (fun (result, (id, testnet, usIn, usOut, usDiff)) ->
       { id ; testnet ; result ; usIn ; usOut ; usDiff })
    (union [
        case (merge_objs (obj1 (req "result" e)) response_encoding)
          (function (Ok v, meta) -> Some (v, meta) | _ -> None)
          (fun (r, meta) -> Ok r, meta) ;
        case (merge_objs (obj1 (req "error" msg_encoding)) response_encoding)
          (function (Error e, meta) -> Some (e, meta) | _ -> None)
          (fun (r, meta) -> Error r, meta) ;
      ])

type 'a notification = {
  channel: channel ;
  data: 'a ;
}

let notification_encoding e =
  conv
    (fun { channel ; data } -> channel, data)
    (fun (channel, data) -> { channel ; data })
    (obj2
       (req "channel" channel_encoding)
       (req "data" e))

let update_encoding e =
  conv
    (fun p -> (), (), p)
    (fun ((), (), p) -> p)
    (obj3
       (req "jsonrpc" (constant "2.0"))
       (req "method" (constant "subscription"))
       (req "params" (notification_encoding e)))

type quote = {
  action: [`New | `Change | `Delete] ;
  price: float ;
  qty: int64 ;
} [@@deriving sexp]

let action_encoding =
  string_enum [
    "new", `New ;
    "change", `Change ;
    "delete", `Delete ;
  ]

let quote_encoding =
  conv
    (fun { action ; price ; qty } -> action, price, qty)
    (fun (action, price, qty) -> { action ; price ; qty })
    (tup3 action_encoding float int53)

type book = {
  ts: Ptime.t ;
  symbol: string ;
  bids: quote list ;
  asks: quote list ;
  chgID: int64 ;
  action: [`Partial | `Update of int64]
} [@@deriving sexp]

let book_encoding =
  let open Json_encoding in
  conv
    (fun { ts ; symbol ; bids ; asks ; chgID ; action } ->
       let prevChgID =
         match action with `Partial -> None | `Update i -> Some i in
       (ts, symbol, bids, asks, chgID, prevChgID))
    (fun (ts, symbol, bids, asks, chgID, prevChgID) ->
       let action = match prevChgID with
         | None -> `Partial
         | Some i -> `Update i in
       { ts ; symbol ; bids ; asks ; chgID ; action })
    (obj6
       (req "timestamp" Ptime.encoding)
       (req "instrument_name" string)
       (req "bids" (list quote_encoding))
       (req "asks" (list quote_encoding))
       (req "change_id" int53)
       (opt "prev_change_id" int53))

type trade = {
  symbol: string ;
  id: string ;
  seq: int64 ;
  ts: Ptime.t ;
  price: float ;
  indexPrice: float ;
  iv: float option ;
  size: int64 ;
  side: Fixtypes.Side.t ;
  tickDirection: Fixtypes.TickDirection.t ;
  liquidation: [`Maker | `Taker | `Both] option ;
} [@@deriving sexp]

let side_encoding =
  string_enum [
    "buy", Fixtypes.Side.Buy ;
    "sell", Sell ;
  ]

let liquidation_encoding =
  string_enum [
    "M", `Maker ;
    "T", `Taker ;
    "MT", `Both ;
  ]

let tickDirection_encoding =
  conv
    (function
      | Fixtypes.TickDirection.PlusTick -> 0
      | ZeroPlusTick -> 1
      | MinusTick -> 2
      | ZeroMinusTick -> 3)
    (function
      | 0 -> PlusTick
      | 1 -> ZeroPlusTick
      | 2 -> MinusTick
      | _ -> ZeroMinusTick)
    (ranged_int ~minimum:0 ~maximum:3 "tickDirection")

let trade_encoding =
  let open Json_encoding in
  conv
    (fun { symbol ; id ; seq ; ts ; price ; indexPrice ;
           iv ; size ; side ; tickDirection ; liquidation } ->
      (id, (seq, ts, symbol, price, indexPrice, iv, size, side, tickDirection, liquidation)))
    (fun (id, (seq, ts, symbol, price, indexPrice, iv, size, side, tickDirection, liquidation)) ->
       { symbol ; id ; seq ; ts ; price ; indexPrice ;
         iv ; size ; side ; tickDirection ; liquidation })
    (merge_objs
       (obj1 (req "trade_id" string))
       (obj10
          (req "trade_seq" int53)
          (req "timestamp" Ptime.encoding)
          (req "instrument_name" string)
          (req "price" float)
          (req "index_price" float)
          (opt "iv" float)
          (req "amount" int53)
          (req "direction" side_encoding)
          (req "tick_direction" tickDirection_encoding)
          (opt "liquidation" liquidation_encoding)))

type t =
  | Subscribe of channel list request
  | Unsubscribe of channel list request
  | Subscriptions of channel list response
  | Quotes of book
  | Trades of trade list
[@@deriving sexp]

let encoding =
  let open Json_encoding in
  union [
    case pub_subscribe_encoding
      (function Subscribe a -> Some a | _ -> None)
      (fun a -> Subscribe a) ;
    case pub_unsubscribe_encoding
      (function Unsubscribe a -> Some a | _ -> None)
      (fun a -> Unsubscribe a) ;
    case (response_encoding (list channel_encoding))
      (function Subscriptions subs -> Some subs | _ -> None)
      (fun a -> Subscriptions a) ;
    case (update_encoding (list trade_encoding))
      (function Trades t -> Some { channel = Trades "" ; data = t } | _ -> None)
      (fun { channel = _ ; data } -> Trades data) ;
    case (update_encoding book_encoding)
      (function Quotes t -> Some { channel = Book "" ; data = t } | _ -> None)
      (fun { channel = _ ; data } -> Quotes data) ;
  ]

let pp ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)
