open Hashtbl;;
open Queue;;

exception Working_list_is_empty ;;

type 'a working_list = {indice:int;
                        clean   : unit->unit ;
			pop     : unit->'a   ;
			push    : 'a -> unit ;
			list    : unit -> 'a list;
			exists  : ('a->bool)->bool;
                        member  : 'a -> bool;
                        not_empty : unit->bool;
                        copy : unit->'a working_list};;



let rec new_working_list n =
    let h=Hashtbl.create n in
    let l=Queue.create () in
    let push a =
	try (Hashtbl.find h a) with _ -> (Queue.add a l ;
					  Hashtbl.add h a ()) in
    let pop () =
        try (let k=Queue.take l in
             Hashtbl.remove h k;k) with _ -> raise Working_list_is_empty in
    let not_empty () =
        try (let a=pop ()in push a;true) with _ -> false in
    let clean () =
        try (while true do pop ();() done) with _ -> () in
    let list () =
        let rep=ref [] in
        iter (fun x->rep:=x::(!rep)) l;
        !rep in
    let copy x =
        let rep = new_working_list n in
        iter (fun x->rep.push x) l;
        rep in
    let member x  =
        try (Hashtbl.find h x;true) with _ -> false in
    let exists p =
        try (iter (fun x->if p x then raise Exit else ()) l;false)
	with _ -> true in
    {indice=n;
     clean=clean;
     list=list;
     pop=pop;
     push=push;
     exists=exists;
     member=member;not_empty=not_empty;copy=copy };;


let wmap f l =
  let rep=new_working_list Config_aff.hashnumber in
  List.map (fun x->rep.push (f x)) (l.list ())
