type node = int

let node_of_int x = x
let int_of_node (x:node) = (x:int)

module NodeSetMap =
  (SetMap.Make
       (struct
         type t = node
         let compare = compare
         let print = Format.pp_print_int
       end
       ))
module NodeMap = NodeSetMap.Map

module Fixed_size_array =
  (
    Int_storage.Quick_key_list(Int_storage.Int_storage_imperatif) :
      Int_storage.Storage
    with type key = node
     and type dimension = int
  )

module Nodearray =
  (
    Int_storage.Nearly_inf_Imperatif :
      Int_storage.Storage
    with type key = node
     and type dimension = int
  )

type ('node_labels,'edge_labels) graph =
  {
    node_labels: 'node_labels Fixed_size_array.t ;
    edges: (node * 'edge_labels) list Fixed_size_array.t ;
  }

let create parameters error node_of_node_label node_list edge_list =
  let max_node =
    List.fold_left
      (fun m i -> max m (int_of_node i))
      0 node_list
  in
  let error, nodes = Fixed_size_array.create parameters error max_node  in
  let error, nodes =
    List.fold_left
      (fun (error, nodes) i ->
         Fixed_size_array.set parameters error (i:node) (node_of_node_label i) nodes)
      (error, nodes)
      (List.rev node_list)
  in
  let error, edges = Fixed_size_array.create parameters error max_node in
  let add_edge parameters error (n1,label,n2) edges =
    let error, old  =
      match Fixed_size_array.unsafe_get parameters error n1 edges with
      | error, None -> error, []
      | error, Some a -> error, a
    in
    Fixed_size_array.set parameters error n1 ((n2,label)::old) edges
  in
  let error, edges =
    List.fold_left
      (fun (error,edges) edge ->
         add_edge parameters error edge edges)
      (error, edges)
      edge_list
  in
  {
    node_labels = nodes ;
    edges =  edges ;
  }

let get parameters error i t =
  match Nodearray.unsafe_get parameters error i t
  with error, Some i -> error, i
     | error, None -> error, -1

let add_bridges ?low ?pre parameters error n_to_string e_to_string graph bridges =
  let error, low, pre =
    match low,pre with
    | Some low, Some pre -> error, low,pre
    | None, _ | _, None  ->
      let error, max_node =
        Fixed_size_array.fold
          parameters error
          (fun _parameter error i _ j -> error, max (int_of_node i) j)
          graph.node_labels
          0
      in
      let error, low =
        match low with
        | Some low -> error, low
        | None -> Nodearray.create parameters error 1
      in
      let error, pre =
        match pre with
        | Some pre -> error, pre
        | None -> Nodearray.create parameters error 1
      in
      error, low, pre
  in
  let rec aux parameters error pre low counter bridges label_opt u v =
    let error, pre  =
      Nodearray.set parameters error v counter pre
    in
    let error, low  =
      Nodearray.set parameters error v counter low
    in
    let counter = succ counter in
    let error, edges_v =
      match Fixed_size_array.unsafe_get parameters error v graph.edges with
      | error, None ->
        error, []
      | error, Some a ->
        error, a
    in
    let error, (pre, low, counter, bridges)
      =
      List.fold_left
      (fun
        (error, (pre, low, counter, bridges))
        (w,label) ->
        let error, pre_w = get parameters error w pre in
        let error, (pre, low, counter, bridges) =
          if pre_w = -1
          then
            aux parameters error pre low counter bridges (Some label) v w
          else
            error, (pre, low, counter, bridges)
        in
        let error, low_v = get parameters error v low in
        let error, low_w = get parameters error w low in
        let error, low =
          Nodearray.set parameters error v (min low_v low_w) low
        in
          let error, low_w = get parameters error w low in
        let error, pre_w = get parameters error w pre in
            if pre_w = low_w && w<>u
            then
              let error, l_v_opt =
                Fixed_size_array.get parameters error v graph.node_labels
              in
              let error, l_w_opt =
                Fixed_size_array.get parameters error w graph.node_labels
              in
              match l_v_opt, l_w_opt
              with
              | None,_ | _,None ->
                Exception.warn
                  parameters error
                  __POS__ Exit (pre, low, counter, bridges)
              | Some l_v, Some l_w ->
                error, (pre,low,counter,(l_v,label,l_w)::bridges)
            else
              error, (pre, low, counter, bridges)
      )
      (error, (pre, low, counter, bridges))
      edges_v in
        error, (pre, low, counter, bridges)

  in
  let error, (pre, low, counter, bridges) =
    Fixed_size_array.fold
      parameters error
      (fun parameters error  v _ ( pre, low, counter,bridges) ->
         let error, pre_v = get parameters error v pre in
         if pre_v = -1 then
           aux parameters error pre low counter bridges None v v
         else
           error, (pre, low, counter, bridges))
      graph.node_labels
      (pre, low, 1, bridges)
  in
  let () =
    Printf.fprintf stdout "PRE"
  in
  let _ =
    Nodearray.iter
      parameters error
      (fun parameters error i j ->
         let () = Printf.fprintf stdout "%i,%i," i j in error)
      pre
  in
  let () = Printf.fprintf stdout "\n" in
  let () =
    Printf.fprintf stdout "LOW"
  in
  let _ =
    Nodearray.iter
      parameters error
      (fun parameters error i j ->
         let () = Printf.fprintf stdout "%i,%i," i j in error)
      low
  in
  let () = Printf.fprintf stdout "\n" in

  let error, (pre, low) =
    Fixed_size_array.fold
      parameters error
      (fun parameters error v _ (pre, low) ->
         let error, pre = Nodearray.set parameters error v (-1) pre in
         let error, low = Nodearray.set parameters error v (-1) low in
         error, (pre, low))
      graph.node_labels
      (pre, low)
  in
  error, (pre, low, bridges)
