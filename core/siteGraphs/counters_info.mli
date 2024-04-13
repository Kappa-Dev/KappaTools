type translate_int = BASIS_MINUS_INPUT of int

type conversion_info =
  {
    from_sig_name: string Loc.annoted;
    convert_value: translate_int;
    convert_delta: translate_int;
  }

type origin = From_original_ast | From_clte_elimination of conversion_info

val apply_int: translate_int -> int -> int
val apply_origin_to_value: origin -> int -> int
val apply_origin_to_delta: origin -> int -> int

val reorder_bounds: translate_int -> ('a * 'a ) -> ('a * 'a)

type counter_sig = {
  counter_sig_name: string Loc.annoted;
  counter_sig_min: int option Loc.annoted option;
  counter_sig_max: int option Loc.annoted option;
  counter_sig_visible: origin;
  counter_sig_default: int;
}


type t = counter_sig option array array

val print_counter : t -> int -> int -> Format.formatter -> unit
val print_kappa :
  noCounters:bool -> Signature.s -> Format.formatter -> t -> unit
val to_yojson : t -> Yojson.Basic.t
val of_yojson : Yojson.Basic.t -> t


val get_conversion_info: ?except:exn -> counter_sig -> conversion_info
val get_counter_sig:  ?except:exn ->
            Signature.s -> t -> int -> int -> counter_sig
