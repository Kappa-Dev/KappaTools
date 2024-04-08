module S = struct
  type 'a t = {
    value: 'a ref;
    hooks: ('a -> unit) list ref;
    eq: 'a -> 'a -> bool;
    signal: 'a React.signal;
    set_signal: ?step:React.step -> 'a -> unit;
  }

  let create ?(eq : 'a -> 'a -> bool = ( = )) (a : 'a) : 'a t =
    let signal, set_signal = React.S.create a in
    { value = ref a; hooks = ref []; eq; signal; set_signal }

  let register hooked f = hooked.hooks := f :: !(hooked.hooks)
  let v hooked = !(hooked.value)

  let set hooked value =
    if not (hooked.eq value !(hooked.value)) then (
      List.iter (fun f -> f value) !(hooked.hooks);
      hooked.value := value;
      hooked.set_signal value
    )

  let bind ?eq hooked f =
    let new_hooked = create ?eq (f (v hooked)) in
    register hooked (fun value -> set new_hooked (f value));
    new_hooked

  let to_react_signal hooked = hooked.signal
end

module E = struct
  type 'a t = {
    hooks: ('a -> unit) list ref;
    event: 'a React.event;
    send_event: ?step:React.step -> 'a -> unit;
  }

  let create () : 'a t =
    let event, send_event = React.E.create () in
    { hooks = ref []; event; send_event }

  let register hooked f = hooked.hooks := f :: !(hooked.hooks)

  let send hooked value =
    List.iter (fun f -> f value) !(hooked.hooks);
    hooked.send_event value

  let map hooked f =
    let new_hooked = create () in
    register hooked (fun value -> send new_hooked (f value));
    new_hooked

  let to_react_event hooked = hooked.event
end
