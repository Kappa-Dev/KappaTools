module A = LargeArray.GenArray
module S = Mods.IntSet 
module M = Mods.IntMap 

let closure prec = 
  let max_index = 
    M.fold 
      (fun i _ -> max i)
      prec 
      0 
  in 
  let n_pred = A.create (max_index+1) 0 in 
  let l_succ = A.create (max_index+1) [] in 
  let l_pred = A.create (max_index+1) [] in 
  let s_pred_star = A.create (max_index+1) S.empty in 
  let add (pred:int) (succ:int) = 
    let _ = A.set l_succ pred (succ::(A.get l_succ pred )) in 
    let _ = A.set n_pred succ (1+(A.get n_pred succ)) in 
    ()
  in 
  let remove pred succ to_do = 
    let k = (A.get n_pred succ)-1 in 
    let _ = A.set n_pred succ k in 
    if k = 0 
    then 
      succ::to_do 
    else 
      to_do 
  in 
  let _ = 
    M.iter  
      (fun succ s_pred -> 
        let l = S.elements s_pred in 
        let _ = A.set l_pred succ l in 
        List.iter 
          (fun pred -> add pred succ)
          l 
      )
      prec 
  in 
  let l = ref [] in 
  let _  = 
    A.iteri  
      (fun i n  -> if n=0 then l:=(i::(!l)))
      n_pred 
  in 
  let to_do = !l in 
  let rec aux to_do = 
    match to_do with 
    | [] -> s_pred_star
    | succ::to_do ->
      begin
        let pred_star = 
          List.fold_left 
            (fun pred_star pred -> 
              S.add pred (S.union pred_star (A.get s_pred_star pred)))
            S.empty
            (A.get l_pred succ)
        in 
        let _ = 
          A.set s_pred_star succ pred_star 
        in 
        let to_do = 
          List.fold_left 
            (fun to_do succ_succ -> remove succ succ_succ to_do)
            to_do 
            (A.get l_succ succ)
        in 
        aux to_do 
      end 
  in 
  let t = aux to_do in 
  let m = ref M.empty in 
  let _ = 
    A.iteri 
      (fun i s -> m:= M.add i s (!m))
      t
  in 
  !m

      

