open Sexplib.Std
open Json_encoding
open Deribit

let scheme = "https"
let host = "www.deribit.com"
let testnet_host = "test.deribit.com"
let path = "ws/api/v2"

let url = Uri.make ~scheme ~host ~path ()
let url_test = Uri.make ~scheme ~host:testnet_host ~path ()

type channel =
  | Trades of string
  | Book of string
  | Perp of string
  | PriceIndex of string
  | PriceRanking of string
[@@deriving sexp]

let trade_chan sym = Trades sym
let book_chan sym = Book sym
let perp_chan sym = Perp sym
let index_chan sym = PriceIndex sym
let ranking_chan sym = PriceRanking sym

let channel_of_string s =
  match String.split_on_char '.' s with
  | ["trades"; instr; "raw"] -> Some (Trades instr)
  | ["book"; instr; "raw"] -> Some (Book instr)
  | ["perpetual"; instr; "raw"] -> Some (Perp instr)
  | ["deribit_price_index"; instr] -> Some (PriceIndex instr)
  | ["deribit_price_ranking"; instr] -> Some (PriceRanking instr)
  | _ -> None

let channel_of_string_exn s =
  match channel_of_string s with
  | Some c -> c
  | None -> invalid_arg "channel_of_string"

let channel_encoding =
  conv
    (function
      | Trades instr -> "trades." ^ instr ^ ".raw"
      | Book instr -> "book." ^ instr ^ ".raw"
      | Perp instr -> "perpetual." ^ instr ^ ".raw"
      | PriceIndex instr -> "deribit_price_index." ^ instr
      | PriceRanking instr -> "deribit_price_ranking." ^ instr
    )
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

type perp = {
  ts: Ptime.t ;
  interest: float ;
  indexPrice: float ;
} [@@deriving sexp_of]

let perp =
  conv
    (fun _ -> assert false)
    (fun (ts, interest, indexPrice) ->
       { ts; interest; indexPrice })
    (obj3
       (req "timestamp" Ptime.encoding)
       (req "interest" float)
       (req "index_price" float))

type index = {
  ts: Ptime.t ;
  price: float ;
  symbol: string ;
} [@@deriving sexp_of]

let index =
  conv
    (fun _ -> assert false)
    (fun (ts, price, symbol) ->
       { ts; price; symbol })
    (obj3
       (req "timestamp" Ptime.encoding)
       (req "price" float)
       (req "index_name" string))

type ranking = {
  weight: float ;
  ts: Ptime.t ;
  price: float option ;
  origPrice: float option ;
  id: string ;
  enabled: bool ;
} [@@deriving sexp_of]

let ranking =
  conv
    (fun _ -> assert false)
    (fun (weight, ts, price, origPrice, id, enabled) ->
       { weight; ts; price; origPrice; id; enabled })
    (obj6
       (req "weight" float)
       (req "timestamp" Ptime.encoding)
       (req "price" (option float))
       (req "original_price" (option float))
       (req "identifier" string)
       (req "enabled" bool))

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
  | Perp of string * perp
  | PriceIndex of index
  | PriceRanking of ranking list
[@@deriving sexp_of]

let encoding =
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
    case (update_encoding perp)
      (fun _ -> assert false)
      (function { channel = Perp instr; data } -> Perp (instr, data)
              | _ -> assert false) ;
    case (update_encoding index)
      (fun _ -> assert false)
      (function { channel = PriceIndex _; data } -> PriceIndex data
              | _ -> assert false) ;
    case (update_encoding (list ranking))
      (fun _ -> assert false)
      (function { channel = PriceRanking _; data } -> PriceRanking data
              | _ -> assert false) ;
  ]

let pp ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)
