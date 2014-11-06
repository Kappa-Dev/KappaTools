type pos = string*int*int

let ln (_,i,_) = i
let cn (_,_,j) = j
let fn (n,_,_) = n

let no_pos = ("",-1,-1)

let string_of_pos = fun (n,i,j) -> ("(in "^n^") line "^(string_of_int i)^", char "^(string_of_int j)^": ")

let string_of_set f fold set = 
  let l = 
    fold (fun i cont -> (f i)::cont) set [] 
  in
    Printf.sprintf "{%s}" (String.concat "," l)

let string_of_map ?(swap=false) f1 f2 fold map = 
  let l = 
    fold (fun i j cont -> 
			if swap then ((f2 j)^"->"^(f1 i))::cont
			else ((f1 i)^"->"^(f2 j))::cont) map [] 
  in
    Printf.sprintf "[%s]" (String.concat "," l)

let string_of_array f ar =
	let l = ref [] in 
		Array.iteri (fun i e -> l:=(((string_of_int i)^":"^(f e))::!l)) ar ;
		"[|"^(String.concat ";" (List.rev !l))^"|]"
		
let string_of_list f l =
	"["^(String.concat ";" (List.rev_map f (List.rev l)))^"]"
	
let pow x n =
	let rec aux x n acc =
		if n = 0 then acc
		else
			aux x (n-1) (x*acc)
	in
	aux x n 1
	
let pow64 x n = 
	let rec aux x n acc =
		if n = 0 then acc
		else
			aux x (n-1) (Int64.mul x acc)
	in
	aux x n Int64.one
	

(*number of bits used to represent n in base 2*)
let bit_rep_size n = 
	let rec aux p acc = 
		if p = 0 then acc
		else
			let p' = p/2 in
			aux p' (acc+1)
	in
		aux n 0  

let replace_space str = 
	let cpt = ref 0 in
	String.iter (fun c -> if c=' ' then String.set str !cpt '_' ; cpt := !cpt+1) str ;
	str

let read_input () = 
	let rec parse acc input =
		match Stream.next input with
			| '\n' -> acc
			| c -> parse (Printf.sprintf "%s%c" acc c) input
	in
	try
		let user_input = Stream.of_channel stdin in
		parse "" user_input
	with
		| Stream.Failure -> invalid_arg "Tools.Read_input: cannot read stream"

let list_of_string str = 
	let stream = Stream.of_string str in
	let rec parse stream acc cont = 
		try
  		match Stream.next stream with
  			| ' ' | '\t' -> parse stream "" (acc::cont)
				| '\n' -> (acc::cont)
  			| c -> parse stream  (Printf.sprintf "%s%c" acc c ) cont
		with Stream.Failure -> (acc::cont)
	in
	parse stream "" []


let array_fold_left_mapi f x a =
  let y = ref x in
  let o = Array.init (Array.length a)
		     (fun i -> let (y',out) = f i !y a.(i) in
			       let () = y := y' in
			       out) in
  (!y,o)

let array_map_of_list f l =
  let len = List.length l in
  let rec fill i v = function
    | [] -> ()
    | x :: l ->
       Array.unsafe_set v i (f x);
       fill (succ i) v l
  in
  match l with
  | [] -> [||]
  | x :: l ->
     let ans = Array.make len (f x) in
     let () = fill 1 ans l in
     ans

let iteri f i =
  let rec aux j =
  if j < i then let () = f j in aux (succ j)
  in aux 0

let find_available_name name ext =
  let base = try Filename.chop_extension name with _ -> name in
  if Sys.file_exists (base^"."^ext) then
          let v = ref 0 in
          let () =
            while Sys.file_exists (base^"~"^(string_of_int !v)^"."^ext) do incr v; done
          in base^"~"^(string_of_int !v)^"."^ext
   else
        (base^"."^ext)

(**[build_fresh_filename base_name l ext] returns a filename that does 
not exists in the working directory using [base_name] appended to strings 
in [l] and adding the extension [ext] at the end*)
let build_fresh_filename base_name concat_list ext =
        let tmp_name = 
                try Filename.chop_extension base_name with _ -> base_name
        in
        let base_name = String.concat "_" (tmp_name::concat_list) in
        find_available_name base_name ext

