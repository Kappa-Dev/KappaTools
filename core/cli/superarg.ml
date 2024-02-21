(* Similar to OCaml's standard Arg module but with some improvements:
   - options are classified with respect to categories
   - neophyte / expert modes
   - hidden options

   Note that the spec type is really different.

   Copyright (C) Antoine Mine' 2006
*)

let expert_mode = ref false (* shows expert options *)
let dev_mode = ref false (* accept _ALL_ options *)

(* Types *)
(* ***** *)

type key = string (* command-line argument *)
type msg = string (* help message *)

(* type of option *)
type level =
  | Normal (* always shown & accepted *)
  | Expert (* shown in expert or developper mode, always accepted *)
  | Developper (* only shown & accepted in developper mode *)
  | Hidden (* never shown *)

module StringInt = struct
  type t = string * int * level option

  let compare (a, b, _) (c, d, _) =
    let cmp = compare b d in
    if cmp = 0 then
      compare a c
    else
      cmp

  let print fmt (a, b, _) = Format.fprintf fmt "%s:%i" a b
end

module StringIntSetMap = SetMap.Make (StringInt)
module StringIntMap = StringIntSetMap.Map
module StringIntSet = StringIntSetMap.Set
module StringSetMap = Mods.StringSetMap
module StringSet = StringSetMap.Set
module StringMap = StringSetMap.Map

let min_level a b =
  match a, b with
  | Normal, _ | _, Normal -> Normal
  | Expert, _ | _, Expert -> Expert
  | Developper, _ | _, Developper -> Developper
  | Hidden, Hidden -> Hidden

let max_level a b =
  match a, b with
  | Hidden, _ | _, Hidden -> Hidden
  | Developper, _ | _, Developper -> Developper
  | Expert, _ | _, Expert -> Expert
  | Normal, Normal -> Normal

let max_level_opt a b =
  match b with
  | None -> a
  | Some b -> max_level a b

type position = int
type category = string * position * level option

type spec =
  | Void (* To skip a line *)
  | Bool of bool ref (* Sets a boolean value *)
  | Int of int ref (* Sets an integer value *)
  | Int_opt of int option ref (* Sets an optional integer value *)
  | String of string ref (* Sets a string value *)
  | String_opt of string option ref (* Sets an optional string value *)
  | String_list of string list ref (* Sets a list of strings *)
  | StringNbr_list of
      (string * string) list ref (* Sets a list of pairs of strings *)
  | Float of float ref (* Sets a float value *)
  | Float_opt of float option ref (* Sets an optional  float value *)
  (* one or several options among a list *)
  | Choice of (key * msg) list * key list * key ref
  | Choice_list of (key * msg) list * key list ref
  (* meta-options to set several options at once:
     - 1st option list is appended as-is when the option is used
     - when 2-st list is not empty, the meta option takes an argument that
     is passed to each option in the list
     - the keys in both lists must appear as regular options as well!
  *)
  | Multi of key list * key list
    (* meta-options, take a list of (k,ext):key*string and give to the option k the parameter s^ext *)
  | MultiExt of (key * string) list

type t = (key * spec * msg * (category * position) list * level) list

(* Utilities *)
(* ********* *)

let show_level (lvl : level) : bool =
  match lvl with
  | Normal -> true
  | Expert -> !expert_mode
  | Developper -> !dev_mode && !expert_mode
  | Hidden -> false

let show_level_opt (lvl_opt : level option) : bool =
  match lvl_opt with
  | None -> true
  | Some lvl -> show_level lvl

let accept_level_display (lvl : level) : bool =
  match lvl with
  | Normal | Expert -> true
  | Developper -> !dev_mode
  | Hidden -> false

let accept_level_use (lvl : level) : bool =
  match lvl with
  | Normal | Expert | Hidden -> true
  | Developper -> !dev_mode

let iskey (k : key) = String.length k > 2 && k.[0] = '-' && k.[1] = '-'

(* --XXX => --no-XXX *)
let nokey (k : key) =
  if String.length k > 2 && k.[0] = '-' && k.[1] = '-' then
    "--no-" ^ String.sub k 2 (String.length k - 2)
  else if String.length k > 1 && k.[0] = '-' then
    "-no-" ^ String.sub k 1 (String.length k - 1)
  else
    failwith (k ^ " option (no) does not begin with -- nor -")

(* --XXX => --(no-)XXX *)
let altkey (k : key) =
  if String.length k > 2 && k.[0] = '-' && k.[1] = '-' then
    "--(no-)" ^ String.sub k 2 (String.length k - 2)
  else if String.length k > 1 && k.[0] = '-' then
    "-(no-)" ^ String.sub k 1 (String.length k - 1)
  else
    failwith (k ^ " option (alt) does not begin with -- not - ")

(* cuts a string into space-separated sub-strings *)
let cut_list (s : string) : string list =
  let rec doit accum pos =
    try
      let i = String.index_from s pos ' ' in
      if i = pos then
        doit accum (i + 1)
      else
        doit (String.sub s pos (i - pos) :: accum) (i + 1)
    with Not_found ->
      if pos < String.length s then
        String.sub s pos (String.length s - pos) :: accum
      else
        accum
  in
  List.rev (doit [] 0)

(* order by category *)
let p (_, _, _, l, _) (_, _, _, l', _) =
  match l, l' with
  | [ (_, i) ], [ (_, i') ] -> compare i i'
  | ([] | _ :: _ :: _), _ | _, ([] | _ :: _ :: _) -> assert false

let order (a : t) =
  let ordered = ref StringIntMap.empty in
  List.iter
    (fun (a, b, c, cat, lvl) ->
      if accept_level_use lvl then
        List.iter
          (fun (cat, i) ->
            let asso, old_lvl =
              StringIntMap.find_default ([], Hidden) cat !ordered
            in
            ordered :=
              StringIntMap.add cat
                ((a, b, c, [ cat, i ], lvl) :: asso, min_level lvl old_lvl)
                !ordered)
          cat)
    a;
  ordered := StringIntMap.map (fun (a, b) -> List.sort p a, b) !ordered;
  !ordered

(* sanity checking *)
let check (a : t) =
  (* check duplicates & compute option list *)
  let opts = ref StringSet.empty in
  List.iter
    (fun (key, b, _, _, _) ->
      if b = Void then
        ()
      else if StringSet.mem key !opts || StringSet.mem (nokey key) !opts then
        failwith ("Duplicate option " ^ key);
      opts := StringSet.add key !opts;
      opts := StringSet.add (nokey key) !opts)
    a;
  (* check sub-options in Muli-options *)
  List.iter
    (fun (key, t, _, _, _) ->
      match t with
      | Multi (a, b) ->
        let f =
          List.iter (fun s ->
              if iskey s && not (StringSet.mem s !opts) then
                failwith ("Unknown option " ^ s ^ " in multi-option " ^ key))
        in
        f a;
        f b
      | Void | Int _ | Float _ | Choice _ | Bool _ | Int_opt _ | String _
      | StringNbr_list _ | String_opt _ | String_list _ | Float_opt _
      | Choice_list (_, _)
      | MultiExt _ ->
        ())
    a

(* Command-line interface *)
(* ********************** *)

(* prints a string with breaks instead of space *)
let print_msg f (s : string) =
  Pp.list Pp.space Format.pp_print_string f (cut_list s)

(* document the options on the standard output! *)
let print_option verbose f (key, spec, msg, _cat, _lvl) =
  let key2 = altkey key in
  (match spec with
  | Bool r ->
    Format.fprintf f "  %s    (default: %s)@." key2
      (if !r then
         "enabled"
       else
         "disabled")
  | Void -> ()
  | Int r -> Format.fprintf f "  %s <int>   (default: %i)@." key !r
  | Int_opt r ->
    (match !r with
    | None -> Format.fprintf f "  %s <int>   (default: disabled)@." key2
    | Some i -> Format.fprintf f "  %s <int>   (default: %i)@." key2 i)
  | String r ->
    (match !r with
    | "" -> Format.fprintf f "  %s <name>   (default: disabled)@." key
    | s -> Format.fprintf f "  %s <name>   (default: %s)@." key s)
  | String_opt r ->
    (match !r with
    | None | Some "" ->
      Format.fprintf f "  %s <name>   (default: disabled)@." key2
    | Some s -> Format.fprintf f "  %s <name>   (default %s)@." key2 s)
  | String_list r ->
    (match !r with
    | [] -> Format.fprintf f "  %s <names> ...   (default: disabled)@." key2
    | l ->
      Format.fprintf f "  %s <names> ...   (default %a)@." key2
        (Pp.list Pp.space Format.pp_print_string)
        l)
  | StringNbr_list r ->
    (match !r with
    | [] -> Format.fprintf f "  %s <names> <...   (default: disabled)@." key2
    | l ->
      let rec aux list =
        match list with
        | (h1, h2) :: tail ->
          let () =
            Format.fprintf f "  %s <names> <value> (default %s %s)" key2 h1 h2
          in
          aux tail
        | [] -> Format.fprintf f "@."
      in
      aux l)
  | Float r -> Format.fprintf f "  %s <float>   (default %f)@." key !r
  | Float_opt r ->
    (match !r with
    | None -> Format.fprintf f "  %s <float>   (default: disabled)@." key2
    | Some v -> Format.fprintf f "  %s <float>   (default: %f)@." key2 v)
  | Choice (l, _, r) ->
    Format.fprintf f "  %s @[%a@] @  (default: %s)@." key
      (Pp.list
         (fun f -> Format.fprintf f " |@ ")
         (fun f (key, _msg) -> Format.pp_print_string f key))
      l !r
  | Choice_list (l, r) ->
    Format.fprintf f "  %s @[%a@]@." key2
      (Pp.list Pp.space (fun f (key, _msg) ->
           Format.fprintf f "[%s%s]" key
             (if List.mem key !r then
                " (default)"
              else
                "")))
      l
  | Multi (_, []) -> Format.fprintf f "  %s@." key
  | Multi _ -> Format.fprintf f "  %s <value>@." key
  | MultiExt _ -> Format.fprintf f "  %s <value>@." key);

  (* shows description if in verbose mode *)
  if verbose && msg <> "" then Format.fprintf f "      @[%a@]@." print_msg msg

let print_help (header : bool) (verbose : bool) f (a : t) =
  let nb = ref 0 in

  (* general options *)
  if header then Format.fprintf f "@.General options@.";
  Format.fprintf f "  --help            Verbose help@.";
  incr nb;
  Format.fprintf f "   -h               Short help@.";
  incr nb;
  Format.fprintf f "  --version         Show version number@.";
  incr nb;
  Format.fprintf f "  --gui             GUI to select@.";
  incr nb;
  Format.fprintf f "  --(no-)expert     Expert mode (more options)@.";
  incr nb;
  if verbose && header then Format.fprintf f "@.";

  (* dump *)
  StringIntMap.iter
    (fun (cat, _, lvl_opt) (l, lvl) ->
      if show_level_opt lvl_opt && show_level lvl then (
        if header then Format.fprintf f "%s@." cat;
        List.iter
          (fun ((_, _, _, _, lvl) as x) ->
            if show_level lvl then (
              incr nb;
              print_option verbose f x
            ))
          l;
        if header && verbose then Format.fprintf f "@."
      ))
    (order a);

  Format.fprintf f "(%i options)@." !nb

(* parse the command-line arguments, given as a list of strings,
   returns the list of non-options (filenames) in reverse order)
*)
let parse_list ~with_tk ?title (a : t) (l : string list) : string list =
  let long_help = ref false
  and short_help = ref false
  and show_version = ref false in

  let rec doit accum = function
    | [] -> accum
    | opt :: rem ->
      (* - means no more options: the rest are filenames *)
      if opt = "-" then
        List.rev_append rem accum
      (* help options *)
      else if opt = "-help" || opt = "--help" then (
        long_help := true;
        doit accum rem
      ) else if opt = "-h" then (
        (* shorter list *)
        short_help := true;
        doit accum rem (* version number *)
      ) else if opt = "--version" then (
        show_version := true;
        doit accum rem (* expert *)
      ) else if opt = "--expert" then (
        expert_mode := true;
        doit accum rem
      ) else if opt = "--no-expert" then (
        expert_mode := false;
        doit accum rem (* regular option, starting with "-" *)
      ) else if String.length opt > 1 && opt.[0] = '-' then (
        let ((key, spec, _, _, _) as aa) =
          try
            List.find
              (fun (key, _spec, _msg, _cat, lvl) ->
                accept_level_use lvl && (opt = key || opt = nokey key))
              a
          with Not_found ->
            Format.printf
              "Here is the list of recognized options@.%a@.Unrecognized \
               option: %s@.@."
              (print_help true false) a opt;
            failwith "bad option"
        in
        let rem =
          try
            match spec, rem with
            | Bool r, rem ->
              r := opt = key;
              rem
            | Int r, "" :: rem when opt = key ->
              r := 0;
              rem
            | Int r, v :: rem when opt = key ->
              r := int_of_string v;
              rem
            | Int_opt r, "" :: rem when opt = key ->
              r := None;
              rem
            | Int_opt r, v :: rem when opt = key ->
              r := Some (int_of_string v);
              rem
            | Int_opt r, rem ->
              r := None;
              rem
            | String r, v :: rem when opt = key ->
              r := v;
              rem
            | String r, rem ->
              r := "";
              rem
            | String_opt r, "" :: rem when opt = key ->
              r := None;
              rem
            | String_opt r, v :: rem when opt = key ->
              r := Some v;
              rem
            | String_opt r, rem ->
              r := None;
              rem
            | String_list _, "" :: rem when opt = key -> rem
            | String_list r, v :: rem when opt = key ->
              r := v :: !r;
              rem
            | String_list r, rem ->
              r := [];
              rem
            | StringNbr_list _, "" :: rem when opt = key -> rem
            | StringNbr_list _, [ _ ] when opt = key -> failwith "invalid pair"
            | StringNbr_list r, v' :: v :: rem when opt = key ->
              r := (v, v') :: !r;
              rem
            | StringNbr_list r, rem ->
              r := [];
              rem
            | Float r, "" :: rem when opt = key ->
              r := 0.;
              rem
            | Float r, v :: rem when opt = key ->
              r := float_of_string v;
              rem
            | Float_opt r, "" :: rem when opt = key ->
              r := None;
              rem
            | Float_opt r, v :: rem when opt = key ->
              r := Some (float_of_string v);
              rem
            | Float_opt r, rem ->
              r := None;
              rem
            | Choice (l, l', r), v :: rem when opt = key ->
              if
                (not (List.exists (fun (k, _) -> v = k) l))
                && not (List.exists (fun k -> v = k) l')
              then
                failwith "invalid choice";
              r := v;
              rem
            | Choice_list (l, r), v :: rem when opt = key ->
              if not (List.exists (fun (k, _) -> v = k) l) then
                failwith "invalid choice";
              r := v :: !r;
              rem
            | Choice_list (_l, r), rem ->
              r := [];
              rem
            | Multi (x, []), rem ->
              ignore (doit [] x);
              rem
            | Multi (x, y), v :: rem ->
              ignore (doit [] x);
              ignore (List.fold_left (fun _ l -> doit [] [ l; v ]) accum y);
              rem
            | MultiExt l, v :: rem when opt = key ->
              ignore
                (List.fold_left (fun _ (l, s) -> doit [] [ l; v ^ s ]) accum l);
              rem
            | MultiExt l, rem ->
              ignore
                (List.fold_left
                   (fun _ (l, s) -> doit [] [ nokey l; s ])
                   accum l);
              rem
            | Multi _, [] | (Void | Int _ | Float _ | Choice _), _ ->
              failwith "invalid option or argument"
          with _ ->
            Format.printf "Wrong option or argument for %s@.%a" opt
              (print_option false) aa;
            failwith "bad option"
        in
        doit accum rem
        (* does not start with - => this is filename *)
      ) else
        doit (opt :: accum) rem
  in

  let filenames = doit [] l in
  if !show_version then (
    Format.printf "%s @.(with%s Tk interface)@."
      (match title with
      | None -> Version.version_kasa_full_name
      | Some x -> x)
      (if with_tk then
         ""
       else
         "out");
    exit 0
  ) else if !long_help then (
    Format.printf "%a" (print_help true true) a;
    exit 0
  ) else if !short_help then (
    Format.printf "%a" (print_help true false) a;
    exit 0
  );
  (* List.concat*) filenames (*(List.map Wordexp.wordexp filenames)*)
