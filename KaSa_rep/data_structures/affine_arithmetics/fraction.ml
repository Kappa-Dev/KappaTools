type fraction={num:int;den:int}
type ffraction=Frac of fraction| Infinity | Unknown | Minfinity

let trunc a = 
    match a with
       Frac{num=x;den=y}->x/y
     |  _                 -> raise Exit

let zero = {num=0;den=1}

let pgcd a b =
    let rec aux a b =
        if b=0 then a
        else aux b (a mod b)
    in
    let a,b=(abs a,abs b) in
       if a<b then aux b a
              else aux a b

let reduit {num=n;den=d} =
    if n=0 then {num=0;den=1}
    else let e=pgcd n d in
         let n,d=n/e,d/e in
         if d<0 then {num=(-n);den=(-d)}
                else {num=n;den=d}

let finf a b = ((a.num)*(b.den))<((a.den)*(b.num))
let finfeq a b = ((a.num)*(b.den))<=((a.den)*(b.num))

let fplus a b =
    reduit {num=(a.num)*(b.den)+(a.den)*(b.num);den=(a.den)*(b.den)}

let fmoins a b =
    reduit {num=(a.num)*(b.den)-(a.den)*(b.num);den=(a.den)*(b.den)}

let ffois a b =
    reduit {num=(a.num)*(b.num);den=(a.den)*(b.den)}

let fdiv a b =
    reduit {num=(a.num)*(b.den);den=(a.den)*(b.num)}

let ffdiv a b =
  match (a,b) with
    Frac(a),Frac(b)->Frac(fdiv a b)
  | Infinity,Frac(a) when a.num>0 -> Infinity
  | Infinity,Frac(a) when a.num<0 -> Minfinity
  | Minfinity,Frac(a) when a.num<0 -> Infinity
  | Minfinity,Frac(a) when a.num>0 -> Minfinity
  | Frac(a),Infinity -> Frac{num=0;den=1}
  | Frac(a),Minfinity -> Frac{num=0;den=1}
  | _ -> Unknown


let ffplus a i b =
    if (i.num)=0 then a
    else (let c =  (match b with Unknown -> Unknown
                               | Infinity when i.num>0 -> Infinity
                               | Minfinity when i.num>0 -> Minfinity
                               | Infinity -> Minfinity
                               | Minfinity -> Infinity
                               | Frac b    -> Frac (ffois i b))
          in match a,c with (_,Frac c) when c.num=0 -> a
                         |  (Unknown,_) -> Unknown
                         | _,Unknown   -> Unknown
                         | Infinity,Minfinity -> Unknown
                         | Infinity,_ -> Infinity
                         | Minfinity,Infinity -> Unknown
                         | _,Infinity -> Infinity
                         | Minfinity,_ -> Minfinity
                         | _,Minfinity -> Minfinity
                         | Frac a,Frac b -> Frac(fplus a b))



let ffmin a b =
    match a,b with Unknown,_ | _,Unknown -> raise Exit
  | Minfinity,_ | _,Infinity -> a
  | Frac(x),Frac(y) when ((fmoins x y).num<0) -> a
  |  _ -> b

let ffinf  a b =
    match a,b with Unknown,_ | _,Unknown -> raise Exit
  | Minfinity,_ | _,Infinity -> true
  | Frac(x),Frac(y) when ((fmoins x y).num<0) -> true
  |  _ -> false


let ffmax a b =
    match a,b with Unknown,_ | _,Unknown -> raise Exit
  | Minfinity,_ | _,Infinity -> b
  | Frac(x),Frac(y) when ((fmoins x y).num<0) -> b
  |  _ -> a



let fsup a b =if (finf a b) then b else a
