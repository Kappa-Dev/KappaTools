type pos = string*int*int

let ln (_,i,_) = i
let cn (_,_,j) = j
let fn (n,_,_) = n

let pos_of_lex_pos pos =
  (pos.Lexing.pos_fname, pos.Lexing.pos_lnum,
   pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

let no_pos = ("",-1,-1)

let string_of_pos (n,i,j) =
  ("(in "^n^") line "^(string_of_int i)^", char "^(string_of_int j)^": ")

let pow x n =
  let rec aux x n acc =
    if n = 0 then acc
    else
      aux x (pred n) (x*acc)
  in
  aux x n 1

let pow64 x n =
  let rec aux x n acc =
    if n = Int64.zero then acc
    else
      aux x (Int64.pred n) (Int64.mul x acc)
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
  String.map (fun c -> if c=' ' then '_' else c) str

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

let kasim_path f =
  if Filename.is_relative f && Filename.dirname f = Filename.current_dir_name
  then Filename.concat !Parameter.outputDirName f
  else f

let kasim_open_out f =
  open_out (kasim_path f)

let find_available_name name ext =
  let base = try Filename.chop_extension name
	     with Invalid_argument _ -> name in
  if Sys.file_exists (base^"."^ext) then
    let v = ref 0 in
    let () =
      while Sys.file_exists (base^"~"^(string_of_int !v)^"."^ext)
      do incr v; done
    in base^"~"^(string_of_int !v)^"."^ext
  else
    (base^"."^ext)

let open_out_fresh_filename base_name concat_list ext =
  let tmp_name =
    kasim_path (try Filename.chop_extension base_name
		with Invalid_argument _ -> base_name) in
  let base_name = String.concat "_" (tmp_name::concat_list) in
  open_out (find_available_name base_name ext)

let mk_dir_r d =
  let rec aux d =
    let par = Filename.dirname d in
    let () = if not (Sys.file_exists par) then aux par in
    Unix.mkdir d 0o775 in
  Unix.handle_unix_error aux d
