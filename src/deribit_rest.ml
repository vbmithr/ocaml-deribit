open Sexplib.Std
open Json_encoding
open Fixtypes
open Deribit

let url = Uri.make ~scheme:"https" ~host:"www.deribit.com" ()
let url_test = Uri.make ~scheme:"https" ~host:"test.deribit.com" ()

module Instrument = struct
  type settlementPeriod =
    | Perp
    | Day
    | Week
    | Month
  [@@deriving sexp_of]

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

  type spec_option = {
    kind: PutOrCall.t;
    strike: float;
  } [@@deriving sexp_of]

  let option = string_enum [
      "put", PutOrCall.Put ;
      "call", Call ;
    ]

  type t =
    | Future of spec
    | Option of spec * spec_option
  [@@deriving sexp_of]

  let timeMs =
    conv
      (fun t -> Ptime.to_float_s t *. 1e3)
      (fun t -> Option.get (Ptime.of_float_s (t *. 1e-3)))
      float

  let period =
    string_enum [
      "perpetual", Perp ;
      "day", Day ;
      "week", Week ;
      "month", Month ;
    ]

  let partOne =
    obj7
      (req "instrument_name" string)
      (req "quote_currency" string)
      (req "base_currency" string)
      (req "is_active" bool)
      (req "expiration_timestamp" timeMs)
      (req "creation_timestamp" timeMs)
      (req "settlement_period" period)

  let partTwo =
    obj6
      (req "taker_commission" float)
      (req "maker_commission" float)
      (req "tick_size" float)
      (req "contract_size" int)
      (req "min_trade_amount" float)
      (opt "max_leverage" int)

  let option =
    conv
      (fun _ -> assert false)
      (fun (kind, strike) -> { kind; strike })
      (obj2
         (req "option_type" option)
         (req "strike" float))

  let spec =
    conv
      (fun _ -> assert false)
      (fun ((name, quoteCurrency, baseCurrency,
             active, expiration, creation, settlementPeriod),
            (takerFee, makerFee, tickSize, contractMultiplier,
             minTradeAmount, maxLeverage))->
        { name; quoteCurrency; baseCurrency;
          active; expiration; creation; settlementPeriod;
          takerFee; makerFee; tickSize; contractMultiplier;
          minTradeAmount; maxLeverage })
      (merge_objs partOne partTwo)

  let encoding =
    union [
      case (merge_objs (obj1 (req "kind" (constant "future"))) spec)
        (function Future a -> Some ((), a) | _ -> None)
        (fun ((), a) -> Future a) ;
      case (merge_objs (obj1 (req "kind" (constant "option"))) (merge_objs spec option))
        (function Option (spec, opt) -> Some ((), (spec, opt)) | _ -> None)
        (fun ((), (spec, opt)) -> Option (spec, opt)) ;
    ]

  let response e =
    let open Core in
    conv
      (fun _ -> assert false)
      (fun { result; _ } ->
         Result.map_error result ~f:(fun { code; msg } ->
             Error.createf "%d: %s" code msg))
      (response_encoding e)

  let get ?kind ?(expired=false) sym url =
    let query =
      ["expired", string_of_bool expired;
       "currency", sym;
      ] in
    let query = match kind with
      | None -> query
      | Some SecurityType.Future -> ("kind", "future") :: query
      | Some SecurityType.Option -> ("kind", "option") :: query
      | _ -> assert false in
    let url = Uri.with_path url "api/v2/public/get_instruments" in
    let url = Uri.with_query' url query in
    Fastrest.get (response (list encoding)) url
end
