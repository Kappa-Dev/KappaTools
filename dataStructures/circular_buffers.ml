type 'a t =
  {
    size: int;
    mutable start: int;
    mutable final: int;
    content: 'a array
  }

let create i default =
  let size = max i 2 in
  {
    size = size;
    start = 0;
    final = 0;
    content = Array.make size default
  }

let succ i t =
  if i = t.size-1 then 0 else succ i

let full t = succ t.final t = t.start

let free_one t = t.start <- succ t.start t

let add x t =
  let () = if full t then free_one t in
  let () = t.content.(t.final) <- x in
  let () = t.final <- succ t.final t in
  t

let iter f t =
  let rec aux i =
    if i <> t.final
    then
      let () = f (t.content.(i)) in
      aux (succ i t)
  in
  aux t.start
