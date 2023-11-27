module IntS = Mods.IntSetMap
module LIntS = Map_wrapper.Make (IntS)
module CharS = Mods.CharSetMap
module LCharS = Map_wrapper.Make (CharS)

let p i j = i = j

let proj i =
  if i mod 2 = 0 then
    'a'
  else
    'b'

let monaproj _ b i = b, proj i

module P = SetMap.Proj (IntS) (CharS)
module LP = Map_wrapper.Proj (LIntS) (LCharS)

let map_test remanent parameters =
  let error0 = remanent.Sanity_test_sig.error in
  let f1 = IntS.Map.add 2 4 IntS.Map.empty in
  let f2 = IntS.Map.add 3 5 f1 in
  let f3 = IntS.Map.add 2 4 f2 in
  let f4 = IntS.Map.add 6 8 f3 in
  let f5 = IntS.Map.add 10 12 f4 in
  let error1, f1' = LIntS.Map.add parameters error0 2 4 IntS.Map.empty in
  let f2' = IntS.Map.add 3 5 f1' in
  let error2, f3' = LIntS.Map.overwrite parameters error1 2 4 f2' in
  let error3, f4' = LIntS.Map.add_or_overwrite parameters error2 6 8 f3' in
  let error4, _ = LIntS.Map.add parameters error3 2 9 f4' in
  let error5, f5' = LIntS.Map.overwrite parameters error3 10 12 f4' in
  let f =
    List.fold_left
      (fun map (a, b) -> IntS.Map.add a b map)
      IntS.Map.empty
      [ 1, [ 2; 3 ]; 2, [ 3; 4 ]; 5, [ 6; 7 ]; 8, [ 12; 13 ] ]
  in
  let g = P.proj_map proj [] List.append f in
  let g' =
    CharS.Map.map List.rev
      (P.proj_map proj [] (fun x y -> List.append (List.rev y) x) f)
  in
  let error6, h =
    LP.monadic_proj_map monaproj parameters error5 []
      (fun _ a l l' -> a, List.append l l')
      f
  in
  let error7, h' =
    LP.monadic_proj_map monaproj parameters error6 []
      (fun _ a x y -> a, List.append (List.rev y) x)
      f
  in
  let h' = LCharS.Map.map List.rev h' in
  let error8, i =
    LP.proj_map proj parameters error7 [] (fun l l' -> List.append l l') f
  in
  let error9, i' =
    LP.proj_map proj parameters error8 []
      (fun x y -> List.append (List.rev y) x)
      f
  in
  let i' = LCharS.Map.map List.rev i' in
  [
    ("map1", fun remanent -> remanent, IntS.Map.equal p f1 f1', None);
    ("map2", fun remanent -> remanent, LIntS.Map.equal p f2 f2', None);
    ("map3", fun remanent -> remanent, IntS.Map.equal p f3 f3', None);
    ("map4", fun remanent -> remanent, IntS.Map.equal p f4 f4', None);
    ("map5", fun remanent -> remanent, IntS.Map.equal p f5 f5', None);
    ("nowarn_add", fun remanent -> remanent, error1 == error0, None);
    ("nowarn_overwrite", fun remanent -> remanent, error2 == error1, None);
    ("nowarn_overwrite_or_add", fun remanent -> remanent, error3 == error2, None);
    ("warn_add", fun remanent -> remanent, not (error4 == error3), None);
    ("warn_overwrite", fun remanent -> remanent, not (error5 == error4), None);
    ("nowarn_proj1", fun remanent -> remanent, error6 == error5, None);
    ("nowarn_proj2", fun remanent -> remanent, error7 == error6, None);
    ("nowarn_proj3", fun remanent -> remanent, error8 == error7, None);
    ("nowarn_proj4", fun remanent -> remanent, error8 == error9, None);
    ("proj1", fun remanent -> remanent, CharS.Map.equal p g g', None);
    ("proj2", fun remanent -> remanent, CharS.Map.equal p g h, None);
    ("proj3", fun remanent -> remanent, CharS.Map.equal p g h', None);
    ( "proj4",
      fun remanent ->
        remanent, CharS.Map.find_default [] 'a' g = [ 3; 4; 12; 13 ], None );
    ("proj5", fun remanent -> remanent, CharS.Map.equal p i i', None);
    ("proj6", fun remanent -> remanent, CharS.Map.equal p i g, None);
  ]
