open Sexplib.Std
open Json_encoding

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
    conv
      (fun t -> 1e3 *. Ptime.to_float_s t)
      (fun ts -> match Ptime.of_float_s (ts /. 1e3) with
         | None -> invalid_arg "Ptime.encoding"
         | Some ts -> ts)
      (ranged_float ~minimum:1e12 ~maximum:max_float "us_encoding")

  let us_encoding =
    conv
      (fun t -> 1e6 *. Ptime.to_float_s t)
      (fun ts -> match Ptime.of_float_s (ts /. 1e6) with
         | None -> invalid_arg "Ptime.encoding"
         | Some ts -> ts)
      (ranged_float ~minimum:1e15 ~maximum:max_float "us_encoding")

  let epoch_encoding =
    conv
      (fun t -> Ptime.to_float_s t)
      (fun _ -> Ptime.epoch)
      (ranged_float ~minimum:0. ~maximum:0. "zero")

  let encoding =
    union [
      case epoch_encoding (fun a -> Some a) (fun a -> a) ;
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

type msg = {
  code: int ;
  msg: string
} [@@deriving sexp]

let msg_encoding =
  conv
    (fun { code ; msg } -> ( code, msg))
    (fun (code, msg) -> { code ; msg })
    (obj2
       (req "code" int)
       (req "message" string))

type ('a, 'b) result = ('a, 'b) Result.result =
    Ok of 'a | Error of 'b [@@deriving sexp]

type 'a response = {
  id: float option ;
  testnet: bool ;
  result: ('a, msg) result ;
  usIn: Ptime.t ;
  usOut: Ptime.t ;
  usDiff: Ptime.span ;
} [@@deriving sexp]

let response =
  conv
    (fun (a, b, c, d, e) -> (), a, b, c, d, e)
    (fun ((), a, b, c, d, e) -> a, b, c, d, e)
    (obj6
       (req "jsonrpc" (constant "2.0"))
       (opt "id" float)
       (req "testnet" bool)
       (req "usIn" Ptime.encoding)
       (req "usOut" Ptime.encoding)
       (req "usDiff" Ptime.us_span_encoding))

let response_encoding e =
  conv
    (fun _ -> assert false)
    (fun (result, (id, testnet, usIn, usOut, usDiff)) ->
       { id ; testnet ; result ; usIn ; usOut ; usDiff })
    (union [
        case (merge_objs (obj1 (req "result" e)) response)
          (function (Ok v, meta) -> Some (v, meta) | _ -> None)
          (fun (r, meta) -> Ok r, meta) ;
        case (merge_objs (obj1 (req "error" msg_encoding)) response)
          (function (Error e, meta) -> Some (e, meta) | _ -> None)
          (fun (r, meta) -> Error r, meta) ;
      ])
