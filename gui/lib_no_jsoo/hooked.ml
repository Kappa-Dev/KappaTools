(* TODO: move this as a parameter *)
let debug_printing = false

module type DebugPrint = sig
  val debug_print : string -> unit
end

module type S = sig
  type 'a t

  val create :
    ?debug:string ->
    ?eq:('a -> 'a -> bool) ->
    'a ->
    'a t * (?debug:string -> 'a -> unit)

  val register : 'a t -> ('a -> unit) -> unit
  val value : 'a t -> 'a
  val set : ?debug:string -> 'a t -> 'a -> unit

  val map :
    ?debug:string -> ?eq:('a -> 'a -> bool) -> ('b -> 'a) -> 'b t -> 'a t

  val fmap :
    ?debug:string ->
    ?eq:('a -> 'a -> bool) ->
    ('b -> 'a option) ->
    'a ->
    'b t ->
    'a t

  val on : ?eq:('a -> 'a -> bool) -> bool t -> 'a -> 'a t -> 'a t
  val l2 : ?eq:('a -> 'a -> bool) -> ('b -> 'c -> 'a) -> 'b t -> 'c t -> 'a t

  val of_react_signal :
    ?debug:string -> ?eq:('a -> 'a -> bool) -> 'a React.signal -> 'a t

  val to_react_signal : 'a t -> 'a React.signal

  (* attempt to avoid GC in case TODO: check *)
  (*   val number_linked_signals : unit -> int *)

  val const : 'a -> 'a t
end

module type E = sig
  type 'a t

  val create : ?debug:string -> unit -> 'a t * ('a -> unit)
  val register : 'a t -> ('a -> unit) -> unit
  val send : 'a t -> 'a -> unit
  val map : ?debug:string -> 'a t -> ('a -> 'b) -> 'b t
  val to_react_event : 'a t -> 'a React.event
end

module MakeS (D : DebugPrint) : S = struct
  type 'a t = {
    mutable value: 'a;
    mutable hooks: ('a -> unit) list;
    eq: 'a -> 'a -> bool;
    signal: 'a React.signal;
    set_signal: ?step:React.step -> 'a -> unit;
    debug: string;
    (* store signal result of `map` with original React.signal,
       flagged as a never read record *)
    mutable link_to_original_signal: 'a React.signal option;
  }

  let _debug_string (hooked : 'a t) : string = hooked.debug

  let _log hooked s =
    if debug_printing then
      D.debug_print (Printf.sprintf "[Hooked.S %s] %s" (_debug_string hooked) s)

  let create_no_set ?(debug : string = "unnamed signal")
      ?(eq : 'a -> 'a -> bool = ( = )) (a : 'a) : 'a t =
    let signal, set_signal = React.S.create ~eq a in
    let hooked =
      {
        value = a;
        hooks = [];
        eq;
        signal;
        set_signal;
        debug;
        link_to_original_signal = None;
      }
    in
    _log hooked "create";
    hooked

  let register hooked f =
    _log hooked "register";
    hooked.hooks <- f :: hooked.hooks

  let value hooked =
    _log hooked "value";
    hooked.value

  let set ?(debug = "") hooked value =
    let value_changed = hooked.eq value hooked.value in
    if not value_changed then (
      _log hooked
        (Printf.sprintf "SET WITH CHANGES%s, running %d hooks"
           (if debug <> "" then
              ": " ^ debug
            else
              "")
           (List.length hooked.hooks));
      hooked.value <- value;
      List.iter (fun f -> f value) hooked.hooks;
      hooked.set_signal value
    ) else
      _log hooked "set NO change"

  let create ?(debug : string = "unnamed signal")
      ?(eq : 'a -> 'a -> bool = ( = )) (a : 'a) :
      'a t * (?debug:string -> 'a -> unit) =
    let hooked : 'a t = create_no_set ~debug ~eq a in
    let set_hooked : ?debug:string -> 'a -> unit =
     fun ?(debug : string = "") -> set ~debug hooked
    in
    hooked, set_hooked

  let map ?(debug : string = "unnamed signal") ?(eq : 'a -> 'a -> bool = ( = ))
      f hooked =
    let new_hooked = create_no_set ~debug ~eq (f (value hooked)) in
    _log hooked (Printf.sprintf "bind %s from" (_debug_string new_hooked));
    register hooked (fun value ->
        _log hooked
          (Printf.sprintf "set from bind %s from" (_debug_string new_hooked));
        set
          ~debug:(Printf.sprintf "bind from %s" (_debug_string hooked))
          new_hooked (f value));
    new_hooked

  let fmap ?(debug : string = "unnamed signal") ?(eq : 'a -> 'a -> bool = ( = ))
      f default hooked =
    map ~debug ~eq
      (fun x ->
        match f x with
        | Some v -> v
        | None -> default)
      hooked

  let on ?(eq : 'a -> 'a -> bool = ( = )) cond_hooked default hooked =
    _log hooked
      ("creating `on` with condition bool hooked `" ^ cond_hooked.debug ^ "`");
    let initial_value =
      if value cond_hooked then
        value hooked
      else
        default
    in
    let new_hooked =
      create_no_set
        ~debug:
          ("`on` from `" ^ hooked.debug ^ "` with cond `" ^ cond_hooked.debug
         ^ "`")
        ~eq initial_value
    in
    register cond_hooked (fun (b : bool) ->
        if b then
          set ~debug:"`on` enable" new_hooked (value hooked)
        else
          set ~debug:"`on` disable" new_hooked default);
    register hooked (fun new_value ->
        if value cond_hooked then set ~debug:"`on` update" new_hooked new_value);
    new_hooked

  let l2 ?(eq : 'a -> 'a -> bool = ( = )) (f : 'b -> 'c -> 'a) (hooked1 : 'b t)
      (hooked2 : 'c t) : 'a t =
    let new_hooked =
      create_no_set
        ~debug:("`combine from " ^ hooked1.debug ^ " and " ^ hooked2.debug)
        ~eq
        (f (value hooked1) (value hooked2))
    in
    register hooked1 (fun new_value1 ->
        set ~debug:"`combine` update" new_hooked (f new_value1 (value hooked2)));
    register hooked2 (fun new_value2 ->
        set ~debug:"`combine` update" new_hooked (f (value hooked1) new_value2));
    new_hooked

  (*
  let react_signal_links = ref []

  let number_linked_signals () = List.length !react_signal_links
*)

  let of_react_signal
      ?(debug : string = "unnamed signal created with of_react_signal")
      ?(eq : 'a -> 'a -> bool = ( = )) (signal : 'a React.signal) : 'a t =
    let hooked = create_no_set ~debug ~eq (React.S.value signal) in

    (* Add map to update value in hooked and store it inside struct *)
    let link_to_original_signal =
      Some
        (React.S.map ~eq
           (* could be using `trace` here, but let's be safe with the GC? no clue *)
             (fun new_value ->
             set ~debug:"from of_react_signal signal" hooked new_value;
             new_value)
           signal)
    in

    (*     react_signal_links := link_to_original_signal :: !react_signal_links; *)
    hooked.link_to_original_signal <- link_to_original_signal;
    set ~debug:"update after link to react signal" hooked (React.S.value signal);
    hooked

  let to_react_signal hooked = hooked.signal

  let const v =
    let hooked = create_no_set v in
    register hooked (fun _ ->
        raise (Failure "const hooked.S has received an attempt to be modified"));
    hooked
end

module MakeE (D : DebugPrint) : E = struct
  type 'a t = {
    mutable hooks: ('a -> unit) list;
    event: 'a React.event;
    send_event: ?step:React.step -> 'a -> unit;
    debug: string;
  }

  let _debug_string (hooked : 'a t) : string = hooked.debug

  let _log hooked s =
    if debug_printing then
      D.debug_print (Printf.sprintf "[Hooked.E %s] %s" (_debug_string hooked) s)

  let create_no_send ?(debug : string = "unnamed event") () : 'a t =
    let event, send_event = React.E.create () in
    let hooked = { hooks = []; event; send_event; debug } in
    _log hooked "create";
    hooked

  let register hooked f =
    _log hooked "register";
    hooked.hooks <- f :: hooked.hooks

  let send hooked value =
    _log hooked "send";
    List.iter (fun f -> f value) hooked.hooks;
    hooked.send_event value

  let create ?(debug : string = "unnamed signal") () : 'a t * ('a -> unit) =
    let hooked : 'a t = create_no_send ~debug () in
    let send_hooked : 'a -> unit = send hooked in
    hooked, send_hooked

  let map ?(debug : string = "unnamed event") hooked f =
    let new_hooked = create_no_send ~debug () in
    register hooked (fun value ->
        _log hooked
          (Printf.sprintf "map send %s from" (_debug_string new_hooked));
        send new_hooked (f value));
    _log hooked (Printf.sprintf "map %s from" (_debug_string new_hooked));
    new_hooked

  let to_react_event hooked = hooked.event
end

(* Testing *)
module DebugPrint : DebugPrint = struct
  let debug_print _ = ()
end

module S = MakeS (DebugPrint)
module E = MakeE (DebugPrint)

let%test "signals basic" =
  let a, set_a = S.create 0 in
  set_a 10;
  let b = S.map (fun v -> v + 1) a in
  S.value b = 11

let%test "events basic" =
  let r = ref 0 in
  let event, send_event = E.create () in
  E.register event (fun i -> r := i);
  send_event 10;
  !r = 10

let%test "to_react_signal" =
  let a, set_a = S.create 0 in
  let signal = S.to_react_signal a in
  set_a 10;
  Unix.sleepf 0.1;
  React.S.value signal = 10

let%test "of_react_signal" =
  let s, set_s = React.S.create 0 in
  let h = S.of_react_signal s in
  set_s 10;
  while React.S.value s = 0 do
    Unix.sleepf 0.1
  done;
  Unix.sleepf 0.1;
  S.value h = 10

let%test_module "on" =
  (module struct
    let a, set_a = S.create 0
    let cond, set_cond = S.create true
    let out = S.on cond (-1) a
    let%test _ = S.value out = 0

    let%test _ =
      set_a 10;
      S.value out = 10

    let%test _ =
      set_cond false;
      S.value out = -1

    let%test _ =
      set_a 42;
      S.value out = -1

    let%test _ =
      set_cond true;
      S.value out = 42

    let%test _ =
      set_a 1;
      S.value out = 1

    let%test _ =
      set_cond false;
      S.value out = -1
  end)
