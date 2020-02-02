open Json_encoding
open Fixtypes

val url : Uri.t
val url_test : Uri.t

module Instrument : sig
  type spec = {
    name: string;
    quoteCurrency: string;
    baseCurrency: string;
    active: bool;
    expiration: Ptime.t ;
    creation: Ptime.t ;
    settlementPeriod: settlementPeriod;

    takerFee: float;
    makerFee: float;
    tickSize: float;
    contractMultiplier: int;
    minTradeAmount: float;
    maxLeverage: int option;
  } [@@deriving sexp_of]

  and settlementPeriod =
    | Week
    | Month
    | Perp

  type t =
    | Future of spec
    | Option of spec * spec_option
  [@@deriving sexp_of]

  and spec_option = {
    kind: PutOrCall.t;
    strike: float;
  } [@@deriving sexp_of]

  val encoding : t encoding

  val get :
    ?kind:SecurityType.t ->
    ?expired:bool -> string -> Uri.t ->
    (Fastrest.form, t list) Fastrest.service
end
