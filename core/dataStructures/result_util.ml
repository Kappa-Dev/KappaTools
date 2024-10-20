(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type status =
  [ `OK
  | `Accepted
  | `Created
  | `Bad_request
  | `Conflict
  | `Not_found
  | `Request_timeout ]
(** The subset of [Cohttp.Code.status] we need *)

type message = {
  severity: Logs.level;
  text: string; (*should be an algebraic type*)
  range: Loc.t option;
}

(* Note(AP): On choice of Result_util.t
   I think this definition was an error, as it forces to redefine functions to use the structure, as well as impedes the interaction with lwt results, where we could have used the lwt_result primitives instead of new functions with different argument order, and presence of both Api.result and Api.result Lwt.t (See api_common.mli).
   We could have used instead: `type ('a, 'b) t = ('a, status * 'b) Result.result` or similar with a record or else.
   This is complex as we have 'b which is in some places a record of lists of exceptions  Exception_without_parameter.exceptions_caught_and_uncaught , sometimes made up to record an error, and sometimes a Api.result, with 'b a Result_util.message list.
   (Initially the record also contained a now-removed `message` field, which was unused as the messages were stored in the 'b)

   This is a lot of work for few benefits now, but that makes using results a bit painful.
   Seems best to used Api.result for new stuff in the webapp, update to it when it's not, and maybe keep the result with the exceptions where it's used.
*)

type ('a, 'b) t = { value: ('a, 'b) Result.result; status: status }

let write_severity ob x =
  let () = Buffer.add_char ob '"' in
  let () = Buffer.add_string ob (Logs.level_to_string (Some x)) in
  Buffer.add_char ob '"'

let read_severity p lb =
  match Logs.level_of_string (Yojson.Basic.read_string p lb) with
  | Result.Ok (Some x) -> x
  | Result.Ok None -> raise (Yojson.Json_error "Message of no severity")
  | Result.Error (`Msg x) ->
    raise (Yojson.Json_error ("While reading severity: " ^ x))

let write_status ob = function
  | `OK -> Buffer.add_string ob "200"
  | `Accepted -> Buffer.add_string ob "202"
  | `Created -> Buffer.add_string ob "201"
  | `Bad_request -> Buffer.add_string ob "400"
  | `Conflict -> Buffer.add_string ob "409"
  | `Not_found -> Buffer.add_string ob "404"
  | `Request_timeout -> Buffer.add_string ob "408"

let read_status p lb =
  match Yojson.Basic.read_int p lb with
  | 200 -> `OK
  | 202 -> `Accepted
  | 201 -> `Created
  | 400 -> `Bad_request
  | 409 -> `Conflict
  | 404 -> `Not_found
  | 408 -> `Request_timeout
  | x ->
    raise
      (Yojson.Json_error
         ("Status " ^ string_of_int x ^ " is out of the scope of Kappa"))

let write_message ob { severity; text; range } =
  let () = Buffer.add_char ob '{' in
  let () = JsonUtil.write_field "severity" write_severity ob severity in
  let () = JsonUtil.write_comma ob in
  let () = JsonUtil.write_field "text" Yojson.Basic.write_string ob text in
  let () =
    match range with
    | None -> ()
    | Some r ->
      let () = JsonUtil.write_comma ob in
      JsonUtil.write_field "range" Loc.write_range ob r
  in
  Buffer.add_char ob '}'

let read_message p lb =
  let severity, text, range =
    Yojson.Basic.read_fields
      (fun (s, t, r) key p lb ->
        if key = "severity" then
          read_severity p lb, t, r
        else if key = "text" then
          s, Yojson.Basic.read_string p lb, r
        else if key = "range" then
          s, t, Some (Loc.read_range p lb)
        else
          raise (Yojson.Json_error ("No field " ^ key ^ " expected in message")))
      (Logs.App, "", None) p lb
  in
  { severity; text; range }

let print_message f { range; text; _ } =
  match range with
  | Some range -> Loc.print_annoted Format.pp_print_string f (text, range)
  | None -> Format.pp_print_string f text

let write_t write__ok write__error ob = function
  | { value = Result.Ok x; status } ->
    Buffer.add_string ob "[\"Ok\",";
    write__ok ob x;
    Buffer.add_char ob ',';
    write_status ob status;
    Buffer.add_char ob ']'
  | { value = Result.Error x; status } ->
    Buffer.add_string ob "[\"Error\",";
    write__error ob x;
    Buffer.add_char ob ',';
    write_status ob status;
    Buffer.add_char ob ']'

let string_of_t write__ok write__error ?(len = 1024) x =
  let ob = Buffer.create len in
  write_t write__ok write__error ob x;
  Buffer.contents ob

let read_t_content f p lb =
  let v = f p lb in
  let () = JsonUtil.read_between_spaces Yojson.Basic.read_comma p lb in
  let s = read_status p lb in
  v, s

let read_t read__ok read__error p lb =
  let aux_read_t closing p lb =
    Yojson.Basic.read_space p lb;
    let out =
      Yojson.Basic.map_ident p
        (fun s pos len ->
          Yojson.Basic.read_space p lb;
          Yojson.Basic.read_comma p lb;
          Yojson.Basic.read_space p lb;
          match String.sub s pos len with
          | "Ok" ->
            let v, status = read_t_content read__ok p lb in
            { value = Result.Ok v; status }
          | "Error" ->
            let v, status = read_t_content read__error p lb in
            { value = Result.Error v; status }
          | x ->
            raise
              (Yojson.Json_error
                 ("Field \"" ^ x ^ "\" does not belong to the result type")))
        lb
    in
    Yojson.Basic.read_space p lb;
    closing p lb;
    Yojson.Basic.read_space p lb;
    out
  in
  match Yojson.Basic.start_any_variant p lb with
  | `Edgy_bracket -> aux_read_t Yojson.Basic.read_gt p lb
  | `Double_quote -> raise (Yojson.Json_error "Not of result type")
  | `Square_bracket -> aux_read_t Yojson.Basic.read_rbr p lb

let t_of_string read__ok read__error s =
  read_t read__ok read__error (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let lift ?(ok_status = `OK) ?(error_status = `Bad_request) = function
  | Result.Ok _ as value -> { value; status = ok_status }
  | Result.Error _ as value -> { value; status = error_status }

let fold ~(ok : 'ok -> 'a) ~(error : 'error -> 'a) : ('ok, 'error) t -> 'a =
  function
  | { value = Result.Ok o; _ } -> ok o
  | { value = Result.Error e; _ } -> error e

let bind :
    type ok a err.
    ?overwrite_status:status ->
    ?error_status:status ->
    (ok -> (a, err) Result.result) ->
    (ok, err) t ->
    (a, err) t =
 fun ?overwrite_status ?(error_status = `Bad_request) ok -> function
  | { value = Result.Error _; _ } as e -> e
  | { value = Result.Ok o; status } ->
    (match ok o with
    | Result.Error _ as value -> { value; status = error_status }
    | Result.Ok _ as value ->
      (match overwrite_status with
      | None -> { value; status }
      | Some status -> { value; status }))

let map : type ok a err. (ok -> a) -> (ok, err) t -> (a, err) t =
 fun ok -> function
  | { value = Result.Ok o; status } -> { value = Result.Ok (ok o); status }
  | { value = Result.Error _; _ } as e -> e

let map2 :
    type a b ok err. (a -> b -> ok) -> (a, err) t -> (b, err) t -> (ok, err) t =
 fun f a b ->
  match a, b with
  | { value = Result.Ok a; _ }, { value = Result.Ok b; status } ->
    { value = Result.Ok (f a b); status }
  | ({ value = Result.Error _; _ } as e), _ -> e
  | { value = Result.Ok _; _ }, ({ value = Result.Error _; _ } as e) -> e

let error ?(status = `Bad_request) (error : 'error) : ('ok, 'error) t =
  { value = Result.Error error; status }

let ok ?(status = `OK) (ok : 'ok) : ('ok, 'error) t =
  { value = Result.Ok ok; status }
