(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type tree = {
  mask: (int, int) Hashtbl.t;
  unmask: (int, int) Hashtbl.t;
  mutable new_mask: int;
  mutable inf_list: Mods.IntSet.t;
  size: int;
  weight_of_nodes: float array;
  weight_of_subtrees: float array;
  unbalanced_events_by_layer: int list array;
  unbalanced_events: bool array;
  layer: int array;
  mutable consistent: bool;
}

let mask t i =
  try Hashtbl.find t.mask i
  with Not_found ->
    let m = t.new_mask in
    let () = t.new_mask <- m + 1 in
    let () = Hashtbl.replace t.mask i m in
    let () = Hashtbl.replace t.unmask m i in
    m

let unmask t m =
  try Hashtbl.find t.unmask m
  with Not_found -> invalid_arg "Random_tree: incoherent hash"

let is_infinite i t =
  let i = mask t i in
  Mods.IntSet.mem i t.inf_list

let find i t =
  let i = mask t i in
  t.weight_of_nodes.(i)

let copy t =
  {
    mask = Hashtbl.copy t.mask;
    unmask = Hashtbl.copy t.unmask;
    new_mask = t.new_mask;
    size = t.size;
    (*	total = t.total ;*)
    weight_of_nodes = Array.copy t.weight_of_nodes;
    weight_of_subtrees = Array.copy t.weight_of_subtrees;
    layer = Array.copy t.layer;
    consistent = t.consistent;
    unbalanced_events_by_layer = Array.copy t.unbalanced_events_by_layer;
    unbalanced_events = Array.copy t.unbalanced_events;
    inf_list = Mods.IntSet.empty;
  }

let copy_vect_in t t1 = Array.iteri (fun i a -> t1.(i) <- a) t

let copy_in t1 t2 =
  let () = copy_vect_in t1.weight_of_nodes t2.weight_of_nodes in
  let () = copy_vect_in t1.weight_of_subtrees t2.weight_of_subtrees in
  let () = copy_vect_in t1.layer t2.layer in
  let () = copy_vect_in t1.unbalanced_events t2.unbalanced_events in
  let () =
    copy_vect_in t1.unbalanced_events_by_layer t2.unbalanced_events_by_layer
  in
  let () = t2.consistent <- t1.consistent in
  t2

let is_root i = i = 1

let declare_unbalanced i t =
  let () =
    if not t.unbalanced_events.(i) then (
      let l = t.layer.(i) in
      let () = t.unbalanced_events.(i) <- true in
      t.unbalanced_events_by_layer.(l) <- i :: t.unbalanced_events_by_layer.(l)
    )
  in
  t.consistent <- false

let update_structure t =
  if t.consistent then
    t
  else (
    let n_layer = t.layer.(t.size) in
    let update_structure_aux i =
      let () =
        t.weight_of_subtrees.(i) <-
          (t.weight_of_nodes.(i)
          +. (if 2 * i > t.size then
                0.
              else
                t.weight_of_subtrees.(2 * i))
          +.
          if (2 * i) + 1 > t.size then
            0.
          else
            t.weight_of_subtrees.((2 * i) + 1))
      in
      let () = t.unbalanced_events.(i) <- false in
      if not (is_root i) then (
        let father = i / 2 in
        declare_unbalanced father t
      )
    in
    let rec aux k =
      if k = 0 then
        ()
      else (
        let l = t.unbalanced_events_by_layer.(k) in
        let () = t.unbalanced_events_by_layer.(k) <- [] in
        let () = List.iter update_structure_aux l in
        aux (k - 1)
      )
    in
    let () = aux n_layer in
    let () = t.consistent <- true in
    t
  )

let create n =
  let t_node = Array.make (n + 1) 0. in
  let t_subtree = Array.make (n + 1) 0. in
  let layer = Array.make (n + 1) 0 in
  let () =
    let rec aux k current_layer layer_end =
      if k <= n then
        if k > layer_end then
          aux k (current_layer + 1) ((2 * layer_end) + 1)
        else (
          let () = layer.(k) <- current_layer in
          aux (k + 1) current_layer layer_end
        )
    in
    aux 1 1 1
  in
  let unbalanced_events_by_layer = Array.make (layer.(n) + 1) [] in
  let unbalanced_events = Array.make (n + 1) false in
  {
    size = n;
    (*	total = 0.;*)
    new_mask = 1;
    mask = Hashtbl.create (n + 1);
    unmask = Hashtbl.create (n + 1);
    inf_list = Mods.IntSet.empty;
    consistent = true;
    weight_of_nodes = t_node;
    weight_of_subtrees = t_subtree;
    unbalanced_events_by_layer;
    unbalanced_events;
    layer;
  }

let add i w t =
  let i = mask t i in
  if w < 0. then
    failwith "Negative value forbidden in Random_tree"
  else (
    let w =
      if w = infinity then (
        let () = t.inf_list <- Mods.IntSet.add i t.inf_list in
        0.
      ) else (
        let () = t.inf_list <- Mods.IntSet.remove i t.inf_list in
        w
      )
    in
    (*	let total = t.total -. t.weight_of_nodes.(i) +. w in*)
    let () = t.weight_of_nodes.(i) <- w in
    let () = declare_unbalanced i t in
    () (*t.total <- (max 0.0 total) (*not satisfactory*)*)
  )

let total t =
  if Mods.IntSet.is_empty t.inf_list then (
    let t = update_structure t in
    if t.size = 0 then
      0.
    else
      t.weight_of_subtrees.(1)
  ) else
    infinity

let random rs t =
  match Mods.IntSet.random rs t.inf_list with
  | Some x -> unmask t x, infinity
  | None ->
    let t = update_structure t in
    let a = total t in
    if a <= 0. then
      raise Not_found
    else (
      let r = Random.State.float rs a in
      let rec find i r =
        let node = t.weight_of_nodes.(i) in
        if r < node then
          i, node
        else if 2 * i > t.size then
          raise Not_found
        else (
          let r' = r -. node in
          let lson = 2 * i in
          let rson = (2 * i) + 1 in
          let left = t.weight_of_subtrees.(lson) in
          if r' < left then
            find lson r'
          else if rson > t.size then
            raise Not_found
          else
            find rson (r' -. left)
        )
      in
      let rep, w = find 1 r in
      unmask t rep, w
    )

(* TODO
    weight_of_subtrees: float array ;
   unbalanced_events_by_layer: int list array ;
*)
let debug_print f t =
  let () =
    Format.fprintf f "@[%sconsistent:@ ["
      (if t.consistent then
         ""
       else
         "un")
  in
  let () =
    Hashtbl.iter
      (fun i k ->
        let bal =
          if t.unbalanced_events.(k) then
            "!"
          else
            ""
        in
        let inv =
          if Hashtbl.find t.mask k = i then
            ""
          else
            " not involutive"
        in
        let inf =
          match classify_float t.weight_of_nodes.(k) with
          | FP_infinite when Mods.IntSet.mem k t.inf_list -> ""
          | FP_infinite -> " not in inf_list"
          | _ when not (Mods.IntSet.mem k t.inf_list) -> ""
          | FP_normal | FP_zero | FP_nan | FP_subnormal -> " in inf_list"
        in
        Format.fprintf f "%s%i:%f%s%s,@," bal i t.weight_of_nodes.(k) inf inv)
      t.unmask
  in
  Format.fprintf f "]@]"
