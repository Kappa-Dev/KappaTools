open Queue

type 'a working_list = {
  indice: int;
  clean: unit -> unit;
  pop: unit -> 'a option;
  push: 'a -> unit;
  list: unit -> 'a list;
  exists: ('a -> bool) -> bool;
  member: 'a -> bool;
  not_empty: unit -> bool;
  copy: unit -> 'a working_list;
}

let rec make n =
  let h = Hashtbl.create n in
  let l = Queue.create () in
  let push a =
    try Hashtbl.find h a
    with _ ->
      Queue.add a l;
      Hashtbl.add h a ()
  in
  let pop () =
    try
      let k = Queue.take l in
      Hashtbl.remove h k;
      Some k
    with _ -> None
  in
  let not_empty () =
    match pop () with
    | Some a ->
      let () = push a in
      true
    | None -> false
  in
  let clean () =
    try
      while true do
        let _ = pop () in
        ()
      done
    with _ -> ()
  in
  let list () =
    let rep = ref [] in
    iter (fun x -> rep := x :: !rep) l;
    !rep
  in
  let copy () =
    let rep = make n in
    iter (fun x -> rep.push x) l;
    rep
  in
  let member x =
    try
      Hashtbl.find h x;
      true
    with _ -> false
  in
  let exists p =
    try
      iter
        (fun x ->
          if p x then
            raise Exit
          else
            ())
        l;
      false
    with _ -> true
  in
  { indice = n; clean; list; pop; push; exists; member; not_empty; copy }

let _wmap parameters _error f l =
  let rep = make (Remanent_parameters.get_empty_hashtbl_size parameters) in
  List.map (fun x -> rep.push (f x)) (l.list ())

let indice wl = wl.indice
let clean wl = wl.clean ()
let list wl = wl.list ()
let pop wl = wl.pop ()
let push a wl = wl.push a
let exists p wl = wl.exists p
let not_empty wl = wl.not_empty ()
let copy wl = wl.copy ()
let member a wl = wl.member a
