let debug_printing = false

module S = struct
  type 'a t = {
    mutable value: 'a;
    mutable hooks: ('a -> unit) list;
    eq: 'a -> 'a -> bool;
    signal: 'a React.signal;
    set_signal: ?step:React.step -> 'a -> unit;
    debug: string;
  }

  let _debug_string (hooked : 'a t) : string = hooked.debug

  let _log hooked s =
    if debug_printing then (
      let () =
        Common.debug ~loc:__LOC__
          (Printf.sprintf "[Hooked.S %s] %s" (_debug_string hooked) s)
      in
      ()
    )

  let create ?(debug : string = "unnamed signal")
      ?(eq : 'a -> 'a -> bool = ( = )) (a : 'a) : 'a t =
    let signal, set_signal = React.S.create a in
    let s = { value = a; hooks = []; eq; signal; set_signal; debug } in
    _log s "create";
    s

  let register hooked f =
    _log hooked "register";
    hooked.hooks <- f :: hooked.hooks

  let v hooked =
    _log hooked "value";
    hooked.value

  let set ?(debug = "") hooked value =
    if not (hooked.eq value hooked.value) then (
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

  let bind ?(debug : string = "unnamed signal") ?(eq : 'a -> 'a -> bool = ( = ))
      hooked f =
    let new_hooked = create ~debug ~eq (f (v hooked)) in
    _log hooked (Printf.sprintf "bind %s from" (_debug_string new_hooked));
    register hooked (fun value ->
        _log hooked
          (Printf.sprintf "set from bind %s from" (_debug_string new_hooked));
        set
          ~debug:(Printf.sprintf "bind from %s" (_debug_string hooked))
          new_hooked (f value));
    new_hooked

  let to_react_signal hooked = hooked.signal
end

module E = struct
  type 'a t = {
    mutable hooks: ('a -> unit) list;
    event: 'a React.event;
    send_event: ?step:React.step -> 'a -> unit;
    debug: string;
  }

  let _debug_string (hooked : 'a t) : string = hooked.debug

  let _log hooked s =
    if debug_printing then (
      let () =
        Common.debug ~loc:__LOC__
          (Printf.sprintf "[Hooked.E %s] %s" (_debug_string hooked) s)
      in
      ()
    )

  let create ?(debug : string = "unnamed event") () : 'a t =
    let event, send_event = React.E.create () in
    let e = { hooks = []; event; send_event; debug } in
    _log e "create";
    e

  let register hooked f =
    _log hooked "register";
    hooked.hooks <- f :: hooked.hooks

  let send hooked value =
    _log hooked "send";
    List.iter (fun f -> f value) hooked.hooks;
    hooked.send_event value

  let map ?(debug : string = "unnamed event") hooked f =
    let new_hooked = create ~debug () in
    register hooked (fun value ->
        _log hooked
          (Printf.sprintf "map send %s from" (_debug_string new_hooked));
        send new_hooked (f value));
    _log hooked (Printf.sprintf "map %s from" (_debug_string new_hooked));
    new_hooked

  let to_react_event hooked = hooked.event
end
