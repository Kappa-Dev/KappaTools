type unbounded = Bounded of int | Infinity

let plus x y = 
  match x,y with 
    Bounded(x),Bounded(y) -> Bounded(x+y)
  | _ -> Infinity

let min a b = 
  match a,b with 
      Bounded(x),Bounded(y) -> (if x<y then a else b)
    | Bounded(x),Infinity ->  a
    | _ -> b

let minl l1 = 
 let rec aux q rep = 
   match q with [] -> rep
   | a::b -> aux b (min a rep) 
 in  match l1 with a::b -> aux b a 
 | _ -> raise Exit

let max a b = 
  match a,b with 
    Bounded(x),Bounded(y) -> (if x<y then b else a)
  | _ -> Infinity

let div2 x = 
 match x with 
  Bounded(x) -> Bounded(x/2)
  | _ -> Infinity

let p x y = 
 match x,y with 
   Bounded(x),Bounded(y) -> x>y 
 | _ , Infinity -> false
 | _-> true
