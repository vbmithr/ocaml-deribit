open Deribit

val url : Uri.t
val url_test : Uri.t

type channel =
  | Trades of string
  | Book of string
  | Perp of string
  | PriceIndex of string
  | PriceRanking of string
[@@deriving sexp]

val trade_chan : string -> channel
val book_chan : string -> channel
val perp_chan : string -> channel
val index_chan : string -> channel
val ranking_chan : string -> channel

type quote = {
  action: [`New | `Change | `Delete] ;
  price: float ;
  qty: int64 ;
} [@@deriving sexp]

type book = {
  ts: Ptime.t ;
  symbol: string ;
  bids: quote list ;
  asks: quote list ;
  chgID: int64 ;
  action: [`Partial | `Update of int64]
} [@@deriving sexp]

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

type index = {
  ts: Ptime.t ;
  price: float ;
  symbol: string ;
} [@@deriving sexp_of]

type ranking = {
  weight: float ;
  ts: Ptime.t ;
  price: float option ;
  origPrice: float option ;
  id: string ;
  enabled: bool ;
} [@@deriving sexp_of]

type 'a request = {
  id: int;
  body: 'a ;
} [@@deriving sexp]

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

val encoding : t Json_encoding.encoding
val pp : Format.formatter -> t -> unit
