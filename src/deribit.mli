open Json_encoding

module Ptime : sig
  include module type of Ptime
    with type t = Ptime.t
     and type span = Ptime.span

  val t_of_sexp : Sexplib.Sexp.t -> Ptime.t
  val sexp_of_t : Ptime.t -> Sexplib.Sexp.t
  val encoding : t encoding
  val us_span_encoding : span encoding
end

type msg = {
  code: int ;
  msg: string
} [@@deriving sexp]

type 'a response = {
  id: float option ;
  testnet: bool ;
  result: ('a, msg) result ;
  usIn: Ptime.t ;
  usOut: Ptime.t ;
  usDiff: Ptime.span ;
} [@@deriving sexp]

val response_encoding : 'a encoding -> 'a response encoding
