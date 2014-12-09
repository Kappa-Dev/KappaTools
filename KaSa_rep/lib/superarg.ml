(* Similar to OCaml's standard Arg module but with some improvements:
   - options are classified with respect to categories
   - neophyte / expert modes
   - hidden options

   Note that the spec type is really different.
   
   Copyright (C) Antoine Mine' 2006
 *)

module StringMap  = Set_and_map.Make (struct type t = string let compare=compare end) 

let expert_mode = ref false  (* shows expert options *)
let dev_mode = ref false    (* accept _ALL_ options *)


(* Types *)
(* ***** *)


type key = string  (* command-line argument *)
type msg = string  (* help message *)

(* type of option *)
type level =
    Normal      (* always shown & accepted *)
  | Expert      (* shown in expert or developper mode, always accepted *)
  | Developper  (* only shown & accepted in developper mode *)
  | Hidden      (* never shown *)

type category = string

type spec =
  | Void                                  (* To skip a line *)
  | Bool of bool ref                      (* Sets a boolean value *)
  | Int of int ref                        (* Sets an integer value *)
  | Int_opt of int option ref             (* Sets an optional integer value *)
  | String of string ref                  (* Sets a string value *)
  | String_opt of string option ref       (* Sets an optional string value *)
  | String_list of string list ref        (* Sets a list of strings *)
  | Float of float ref                    (* Sets a float value *)
  | Float_opt of float option ref         (* Sets an optional  float value *)

    (* one or several options among a list *)
  | Choice of (key * msg) list * (key ref)
  | Choice_list of (key * msg) list * (key list ref)

    (* meta-options to set several options at once: 
       - 1st option list is appended as-is when the option is used
       - when 2-st list is not empty, the meta option takes an argument that
       is passed to each option in the list
       - the keys in both lists must appear as regular options as well!
     *)
  | Multi of (key list) * (key list)
    (* meta-options, take a list of (k,ext):key*string and give to the option k the parameter s^ext *)
  | MultiExt of (key*string) list


type t = (key * spec * msg * category list * level) list


(* Utilities *)
(* ********* *)

let show_level (lvl:level) : bool = match lvl with
| Normal -> true
| Expert -> !expert_mode
| Developper -> !dev_mode && !expert_mode
| Hidden -> false

let accept_level (lvl:level) : bool = match lvl with
| Normal | Expert | Hidden -> true
| Developper -> !dev_mode

let iskey (k:key) = String.length k > 2 && k.[0]='-' && k.[1]='-'

(* --XXX => --no-XXX *)
let nokey (k:key) =
  if String.length k > 2 && k.[0]='-' && k.[1]='-' 
  then "--no-"^(String.sub k 2 (String.length k-2))
  else failwith (k^" option does not begin with --")

(* --XXX => --(no-)XXX *)
let altkey (k:key) =
  if String.length k > 2 && k.[0]='-' && k.[1]='-' 
  then "--(no-)"^(String.sub k 2 (String.length k-2))
  else failwith (k^" option does not begin with --")


(* concats a string list with a separator *)
let cat_list (l:string list) (sep:string) : string =
  let b = Buffer.create 12 in
  let rec doit = function
    | [] -> ()
    | [x] -> Buffer.add_string b x
    | x::y -> Buffer.add_string b x; Buffer.add_string b sep; doit y
  in
  doit l; Buffer.contents b

(* cuts a string into space-separated sub-strings *)
let cut_list (s:string) : string list =
  let rec doit accum pos =
    try
      let i = String.index_from s pos ' ' in
      if i=pos then doit accum (i+1)
      else doit ((String.sub s pos (i-pos))::accum) (i+1)
    with Not_found ->
      if pos < String.length s 
      then (String.sub s pos (String.length s-pos))::accum
      else accum
  in
  List.rev (doit [] 0)

(* order by category *)
let error = Exception.empty_error_handler 
let order parameters (a:t) =
  let ordered = ref StringMap.empty_map in
  List.iter (fun (a,b,c,cat,lvl) ->
    if accept_level lvl then
      List.iter 
	(fun cat -> 
	  ordered := snd (StringMap.add_map parameters error cat
	      ((a,b,c,[cat],lvl)::(try snd (StringMap.find_map parameters error cat !ordered) with Not_found -> [])) !ordered)) 
    cat)
    a;
  !ordered

(* sanity checking *)
let check parameters (a:t) =
  (* check duplicates & compute option list *)
  let opts = ref StringMap.empty_set in
  List.iter (fun (key,b,_,_,_) -> 
    if b=Void then () 
    else if StringMap.mem_set key !opts || StringMap.mem_set (nokey key) !opts
    then failwith ("Duplicate option "^key);
    opts := snd (StringMap.add_set parameters error key !opts);
    opts := snd (StringMap.add_set parameters error (nokey key) !opts )) 
      a;
  (* check sub-options in Muli-options *)
  List.iter (fun (key,t,_,_,_) -> match t with
    Multi (a,b) ->
      let f = 
	List.iter (fun s -> 
	if iskey s && not (StringMap.mem_set s !opts)
	then failwith ("Unknown option "^s^" in multi-option "^key))
      in f a; f b
  | _ -> () ) a
      


(* Command-line interface *)
(* ********************** *)


(* prints a list using p for elements and sep for separator *)
let rec print_list 
    (p:  Format.formatter -> 'a -> unit)
    (sep:Format.formatter -> unit -> unit)
    (f:  Format.formatter)
    (s:'a list) 
  : unit
    = match s with
      [] -> ()
    | [a] -> p f a
    | a::b -> p f a; sep f (); print_list p sep f b  

(* prints a string with breaks instead of space *)
let print_msg f (s:string) =
  print_list Format.pp_print_string Format.pp_print_space f (cut_list s)

(* document the options on the standard output! *)
let print_option verbose f (key,spec,msg,cat,lvl) =
  let key2 = altkey key in
  (match spec with
    Bool r ->
      Format.fprintf f "  %s    (default: %s)@." key2
	(if !r then "enabled" else "disabled")
  |  Void -> ()
  | Int r -> Format.fprintf f "  %s <int>   (default: %i)@." key !r
  | Int_opt r ->
      (match !r with
      | None   -> Format.fprintf f "  %s <int>   (default: disabled)@." key2
      | Some i -> Format.fprintf f "  %s <int>   (default: %i)@." key2 i)
  | String r -> 
      (match !r with
      | "" -> Format.fprintf f "  %s <name>   (default: disabled)@." key
      | s  -> Format.fprintf f "  %s <name>   (default: %s)@." key s)
  | String_opt r -> 
      (match !r with
      | None 
      | Some "" -> Format.fprintf f "  %s <name>   (default: disabled)@." key2
      | Some s  -> Format.fprintf f 
	    "  %s <name>   (default %s)@." key2 s)
  | String_list r ->
      (match !r with
      | [] -> Format.fprintf f 
	    "  %s <names> ...   (default: disabled)@." key2
      | l  -> Format.fprintf f "  %s <names> ...   (default %a)@." key2
	    (print_list Format.pp_print_string Format.pp_print_space) l)
  | Float r -> Format.fprintf f "  %s <float>   (default %f)@." key !r
  | Float_opt r ->
      (match !r with
      | None   -> Format.fprintf f "  %s <float>   (default: disabled)@." key2
      | Some v -> Format.fprintf f "  %s <float>   (default: %f)@." key2 v)
  | Choice (l,r) ->
      Format.fprintf f "  %s @[%a@]    (default: %s)@." 
	key
	(print_list 
	   (fun f (key,msg) -> Format.pp_print_string f key)
	   (fun f () -> Format.fprintf f " |@ ")) l
	!r
  | Choice_list (l,r) ->
      Format.fprintf f "  %s @[%a@]@." key2
	(print_list 
	   (fun f (key,msg) ->
	     Format.fprintf f "[%s%s]" key
	       (if List.mem key !r then " (default)" else "")
	   )
	   Format.pp_print_space
	) l
  | Multi (_,[]) -> Format.fprintf f "  %s@." key
  | Multi _ -> Format.fprintf f "  %s <value>@." key
  | MultiExt _ -> Format.fprintf f "  %s <value>@." key
  );
  
  (* shows description if in verbose mode *)
  if verbose && msg<>"" then Format.fprintf f "      @[%a@]@." print_msg msg
  
  

let print_help parameters (header:bool) (verbose:bool) f (a:t) =
  let nb = ref 0 in

  (* general options *)
  if header then Format.fprintf f "@.General options@.";
  Format.fprintf f "  --help            Verbose help@."; incr nb;
  Format.fprintf f "   -h               Short help@."; incr nb;
  Format.fprintf f "  --gui             GUI to select@."; incr nb;
  Format.fprintf f "  --(no-)expert     Expert mode (more options)@."; incr nb;
  if verbose && header then Format.fprintf f "@.";
  
  (* dump *)
  StringMap.iter_map 
    (fun cat l ->
      if header then Format.fprintf f "%s@." cat;
      List.iter 
	(fun ((_,_,_,_,lvl) as x) ->
	  if show_level lvl then (incr nb; print_option verbose f x) 
	) l;
      if header && verbose then Format.fprintf f "@."
    ) (order parameters a);
  
  Format.fprintf f "(%i options)@." !nb



(* parse the command-line arguments, given as a list of strings,
   returns the list of non-options (filenames) in reverse order)
 *)
let parse_list parameters (a:t) (l:string list) : string list =
  let long_help = ref false
  and short_help = ref false in

  let rec doit accum = function
      [] -> accum
    | opt::rem ->

	(* - means no more options: the rest are filenames *)
	if opt="-" then List.rev_append rem accum

	(* help options *)
	else if opt="-help" || opt="--help" then 
	  (long_help := true; doit accum rem)
	else if opt="-h" then  (* shorter list *)
	  (short_help := true; doit accum rem)

        (* expert *)
	else if opt="--expert" then (expert_mode := true; doit accum rem)
	else if opt="--no-expert" then (expert_mode := false; doit accum rem)

        (* regular option, starting with "-" *)
	else if String.length opt > 1 && opt.[0]='-' then
	  let (key,spec,_,_,_)as aa=
	    try
	      List.find 
		(fun (key,spec,msg,cat,lvl) ->
		  (accept_level lvl) &&
		  (opt=key || (opt=(nokey key)))
		) a
	    with Not_found ->
	      Format.printf "Here is the list of recognized options@.%a@.Unrecognized option: %s@.@." (print_help parameters true false) a opt;
	      failwith "bad option"
	  in
	  let rem =
	    try match spec,rem with 
	    | Bool r, rem -> r := (opt=key); rem
	    | Int r, (""::rem) when opt=key -> r := 0; rem
	    | Int r, (v::rem) when opt=key -> r := int_of_string v; rem
	    | Int_opt r, (""::rem) when opt=key -> r := None; rem
	    | Int_opt r, (v::rem) when opt=key -> 
		r := Some (int_of_string v); rem
	    | Int_opt r, rem -> r := None; rem
	    | String r, (v::rem) when opt=key -> r := v; rem
	    | String r, rem -> r := ""; rem
	    | String_opt r, (""::rem) when opt=key -> r := None; rem
	    | String_opt r, (v::rem) when opt=key -> r := Some v; rem
	    | String_opt r, rem -> r := None; rem
	    | String_list r, (""::rem) when opt=key -> rem
	    | String_list r, (v::rem) when opt=key -> r := v::(!r); rem
	    | String_list r, rem -> r := []; rem
	    | Float r, (""::rem) when opt=key -> r := 0.; rem
	    | Float r, (v::rem) when opt=key -> r := float_of_string v; rem
	    | Float_opt r, (""::rem) when opt=key -> r := None; rem
	    | Float_opt r, (v::rem) when opt=key -> 
		r := Some (float_of_string v); rem
	    | Float_opt r, rem -> r := None; rem
	    | Choice (l,r), (v::rem) when opt=key ->
		if not (List.exists (fun (k,_) -> v=k) l) 
		then failwith "invalid choice";
		r := v; rem
	    | Choice_list (l,r), (v::rem) when opt=key ->
		if not (List.exists (fun (k,_) -> v=k) l) 
		then failwith "invalid choice";
		r := v::(!r); rem
	    | Choice_list (l,r), rem -> r := []; rem
	    | Multi (x,[]), rem -> ignore (doit [] x); rem
	    | Multi (x,y), (v::rem) ->
		ignore (doit [] x);
		ignore (List.fold_left (fun accum l -> doit [] [l;v])
			  accum y);
		rem
	    | MultiExt l,v::rem -> 
		ignore (List.fold_left (fun accum (l,s) -> doit [] [l;v^s])
			  accum l);rem
	    | _ -> failwith "invalid option or argument"
		  
	    with _ ->
	      Format.printf "Wrong option or argument for %s@.%a" opt
		(print_option false) aa;
	      failwith "bad option"
	  in
	  doit accum rem
	      
        (* does not start with - => this is filename *)
	else doit (opt::accum) rem
	    
  in 
  let filenames = doit [] l in
  if !long_help then (Format.printf "%a" (print_help parameters true true) a; exit 0)
  else if !short_help then (Format.printf "%a" (print_help parameters true false) a; exit 0);
 (* List.concat*) filenames (*(List.map Wordexp.wordexp filenames)*)


(* MAIN *)
(* **** *)


let parse parameters (a:t) (def:string list ref) =
  check parameters a;
  (* drop the first command-line argument: it is the executable name *)
  let args = List.tl (Array.to_list Sys.argv) in
  (* parse options & get remaining fienames *)
  let rem = parse_list parameters a args in
  if rem<>[] then def := rem
