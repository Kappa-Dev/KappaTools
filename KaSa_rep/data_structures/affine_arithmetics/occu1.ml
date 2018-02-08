type trans = Pro of int | Trans of int*int*int | Transb of int * int *int ;;


let p x y = 
  match x,y with 
    Pro(-1),Pro(-1)-> 0
  |   Pro (-1),_ -> 1
  | _,Pro(-1) -> (-1)
  | Pro a,Pro b -> b-a
  | Pro a,_ -> (1)
  |  _ ,Pro b -> (-1)
  | Trans(x,y,z),Trans(a,b,c) when a<>x -> a-x
  | Trans(x,y,z),Trans(a,b,c) when y<>b -> b-y
  | Trans(x,y,z),Trans(a,b,c) -> c-z
  |  Trans _ ,_ -> 1
  | _,Trans _ -> (-1)
  | Transb(x,y,z),Transb(a,b,c) when a<>x -> a-x
  | Transb(x,y,z),Transb(a,b,c) when y<>b -> b-y
  | Transb(x,y,z),Transb(a,b,c) -> c-z	

let po x y = ((p x y)>0)

let print_trans x = 
  match x with Pro(x) -> print_string "PRO";print_int x;print_newline()
  |  Trans(x,y,z) -> print_string "TRANS";print_int x;
      print_int y;print_int z;print_newline () 
|  Transb(x,y,z) -> print_string "TRANSB";print_int x;
      print_int y;print_int z;print_newline () 
;;

