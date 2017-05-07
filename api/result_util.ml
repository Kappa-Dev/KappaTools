(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type ('a,'b) t = ('a,'b) Result.result

(*let to_yojson f g = function
  | Ok a -> `List [`String "Ok"; f a]
  | Error a -> `List [`String "Error"; g a]

let of_yojson f g = function
  | `List [`String "Ok"; a] -> Ok (f a)
  | `List [`String "Error"; a] -> Error (g a)
  | x -> raise (Yojson.Basic.Util.Type_error ("Not of result type",x))
*)

let write_t write__ok write__error = fun ob -> function
  | Result.Ok x ->
    Bi_outbuf.add_string ob "[\"Ok\",";
        write__ok ob x;
    Bi_outbuf.add_char ob ']'
  | Result.Error x ->
    Bi_outbuf.add_string ob "[\"Error\",";
    write__error ob x;
    Bi_outbuf.add_char ob ']'
let string_of_t write__ok write__error ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_t write__ok write__error ob x;
  Bi_outbuf.contents ob

let read_t read__ok read__error = fun p lb ->
  let aux_read_t closing p lb =
    Yojson.Basic.read_space p lb;
    let out = Yojson.Basic.map_ident p
        (fun s pos len ->
           Yojson.Basic.read_space p lb;
           Yojson.Basic.read_comma p lb;
           Yojson.Basic.read_space p lb;
           match String.sub s pos len with
           | "Ok" -> Result.Ok (read__ok p lb)
           | "Error" -> Result.Error (read__error p lb)
           | x -> raise (Yojson.Json_error ("Field \""^x^
                                            "\" does not belong to the result type"))
        ) lb in
    Yojson.Basic.read_space p lb;
    closing p lb;
    Yojson.Basic.read_space p lb;
    out in
  match Yojson.Basic.start_any_variant p lb with
  | `Edgy_bracket -> aux_read_t Yojson.Basic.read_gt p lb
  | `Double_quote ->
    raise (Yojson.Json_error "Not of result type")
  | `Square_bracket -> aux_read_t Yojson.Basic.read_rbr p lb
let t_of_string read__ok read__error s =
  read_t read__ok read__error (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let map
    ~(ok:'ok -> 'a)
    ~(error:'error -> 'a) : ('ok,'error) t -> 'a
  =
  function
  | Result.Ok o -> ok o
  | Result.Error e -> error e

let bind ~(ok:'ok -> 'a) : ('ok,'error) t -> 'a = function
  | Result.Ok o -> ok o
  | Result.Error e -> `Error e

let error (error:'error ) : ('ok,'error) t = Result.Error error

let ok (ok : 'ok) : ('ok,'error) t = Result.Ok ok
