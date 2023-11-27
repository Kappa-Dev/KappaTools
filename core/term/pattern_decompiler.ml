(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2020 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

let of_snapshot g =
  let out, _ =
    Tools.array_fold_lefti
      (fun node (acc, pack) ag ->
        let ra_type = ag.Snapshot.node_type in
        let ar = Array.length ag.Snapshot.node_sites in
        let ra_ports =
          Array.make ar (Locality.dummy_annot LKappa.LNK_FREE, LKappa.Maintained)
        in
        let ra_ints = Array.make ar LKappa.I_ANY in
        let pack' =
          Tools.array_fold_lefti
            (fun id ((dangling, free_id) as pack) p ->
              let () =
                match p.Snapshot.site_state with
                | None -> ()
                | Some i -> ra_ints.(id) <- LKappa.I_VAL_CHANGED (i, i)
              in
              match p.Snapshot.site_link with
              | None -> pack
              | Some s ->
                (match Mods.Int2Map.pop s dangling with
                | Some va, dangling' ->
                  let () =
                    ra_ports.(id) <-
                      ( Locality.dummy_annot (LKappa.LNK_VALUE (va, (-1, -1))),
                        LKappa.Maintained )
                  in
                  dangling', free_id
                | None, dangling' ->
                  let () =
                    ra_ports.(id) <-
                      ( Locality.dummy_annot
                          (LKappa.LNK_VALUE (free_id, (-1, -1))),
                        LKappa.Maintained )
                  in
                  Mods.Int2Map.add (node, id) free_id dangling', succ free_id))
            pack ag.Snapshot.node_sites
        in
        let ra =
          {
            LKappa.ra_type;
            ra_erased = false;
            ra_ports;
            ra_ints;
            ra_syntax = Some (Array.copy ra_ports, Array.copy ra_ints);
          }
        in
        ra :: acc, pack')
      ([], (Mods.Int2Map.empty, 1))
      g
  in
  out

let patterns_of_mixture ~debugMode contact_map sigs pre_env e =
  let snap = Edges.build_snapshot ~raw:false sigs e in
  let pre_env', acc =
    Snapshot.fold
      (fun (cc_cache, acc) i m ->
        match
          Pattern_compiler.connected_components_sum_of_ambiguous_mixture
            ~debugMode ~compileModeOn:false contact_map cc_cache (of_snapshot m)
        with
        | cc_cache', [ ([| (_, x) |], _) ] ->
          cc_cache', Tools.recti (fun a _ -> x :: a) acc i
        | _ -> assert false)
      (pre_env, []) snap
  in
  pre_env', acc
