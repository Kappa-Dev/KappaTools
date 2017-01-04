type ('a,'annot) link =
  | LNK_VALUE of int * 'annot
  | FREE
  | LNK_ANY
  | LNK_SOME
  | LNK_TYPE of 'a (* port *)
    * 'a (*agent_type*)

type internal = string Location.annot list

type port = {port_nme:string Location.annot;
             port_int:internal;
             port_lnk:(string Location.annot,unit) link Location.annot list;}

type agent = (string Location.annot * port list)

type mixture = agent list

type rule = {
  lhs: mixture ;
  rm_token: ((mixture,string) Alg_expr.e Location.annot
             * string Location.annot) list ;
  bidirectional:bool ;
  rhs: mixture ;
  add_token: ((mixture,string) Alg_expr.e Location.annot
              * string Location.annot) list;
  k_def: (mixture,string) Alg_expr.e Location.annot ;
  k_un:
    ((mixture,string) Alg_expr.e Location.annot *
     (mixture,string) Alg_expr.e Location.annot option) option;
  (*k_1:radius_opt*)
  k_op: (mixture,string) Alg_expr.e Location.annot option ;
  k_op_un:
    ((mixture,string) Alg_expr.e Location.annot *
     (mixture,string) Alg_expr.e Location.annot option) option;
  (*rate for backward rule*)
}

let flip_label str = str^"_op"

type 'alg_expr print_expr =
    Str_pexpr of string Location.annot
  | Alg_pexpr of 'alg_expr Location.annot

type ('mixture,'id) modif_expr =
  | INTRO of
      (('mixture,'id) Alg_expr.e Location.annot * 'mixture Location.annot)
  | DELETE of
      (('mixture,'id) Alg_expr.e Location.annot * 'mixture Location.annot)
  | UPDATE of
      ('id Location.annot *
       ('mixture,'id) Alg_expr.e Location.annot) (*TODO: pause*)
  | UPDATE_TOK of
      ('id Location.annot *
       ('mixture,'id) Alg_expr.e Location.annot) (*TODO: pause*)
  | STOP of ('mixture,'id) Alg_expr.e print_expr list
  | SNAPSHOT of ('mixture,'id) Alg_expr.e print_expr list
  (*maybe later of mixture too*)
  | PRINT of
      ((('mixture,'id) Alg_expr.e print_expr list) *
       (('mixture,'id)  Alg_expr.e print_expr list))
  | PLOTENTRY
  | CFLOWLABEL of (bool * string Location.annot)
  | CFLOWMIX of (bool * 'mixture Location.annot)
  | FLUX of bool * ('mixture,'id) Alg_expr.e print_expr list
  | FLUXOFF of ('mixture,'id) Alg_expr.e print_expr list

type ('mixture,'id) perturbation =
  (('mixture,'id) Alg_expr.bool Location.annot *
   (('mixture,'id) modif_expr list) *
   ('mixture,'id) Alg_expr.bool Location.annot option) Location.annot

type configuration = string Location.annot * (string Location.annot list)

type ('mixture,'id) variable_def =
  string Location.annot * ('mixture,'id) Alg_expr.e Location.annot

type ('mixture,'id) init_t =
  | INIT_MIX of 'mixture
  | INIT_TOK of 'id

type ('mixture,'id) instruction =
  | SIG      of agent
  | TOKENSIG of string Location.annot
  | VOLSIG   of string * float * string (* type, volume, parameter*)
  | INIT     of
      string Location.annot option *
      ('mixture,'id) Alg_expr.e Location.annot *
      ('mixture,'id) init_t Location.annot
  (*volume, init, position *)
  | DECLARE  of ('mixture,'id) variable_def
  | OBS      of ('mixture,'id) variable_def (*for backward compatibility*)
  | PLOT     of ('mixture,'id) Alg_expr.e Location.annot
  | PERT     of ('mixture,'id) perturbation
  | CONFIG   of configuration

type ('mixture,'id) command =
  | RUN of ('mixture,'id) Alg_expr.bool
  | MODIFY of ('mixture,'id) modif_expr list
  | QUIT

type ('agent,'mixture,'id,'rule) compil =
  {
    variables :
      ('mixture,'id) variable_def list;
    (*pattern declaration for reusing as variable in perturbations
      or kinetic rate*)
    signatures :
      'agent list; (*agent signature declaration*)
    rules :
      (string Location.annot option * 'rule Location.annot) list;
    (*rules (possibly named)*)
    observables :
      ('mixture,'id) Alg_expr.e Location.annot list;
    (*list of patterns to plot*)
    init :
      (string Location.annot option *
       ('mixture,'id) Alg_expr.e Location.annot *
       ('mixture,'id) init_t Location.annot) list;
    (*initial graph declaration*)
    perturbations :
      ('mixture,'id) perturbation list;
    configurations :
      configuration list;
    tokens :
      string Location.annot list;
    volumes :
      (string * float * string) list
  }

let no_more_site_on_right warning left right =
  List.for_all
    (fun p ->
       List.exists (fun p' -> fst p.port_nme = fst p'.port_nme) left
       || let () =
            if warning then
              ExceptionDefn.warning
                ~pos:(snd p.port_nme)
                (fun f ->
                   Format.fprintf
                     f "@[Site@ '%s'@ was@ not@ mentionned in@ the@ left-hand@ side."
                     (fst p.port_nme);
                   Format.fprintf
                     f "This@ agent@ and@ the@ following@ will@ be@ removed@ and@ ";
                   Format.fprintf
                     f "recreated@ (probably@ causing@ side@ effects).@]")
       in false)
    right

let empty_compil =
  {
    variables      = [];
    signatures     = [];
    rules          = [];
    init           = [];
    observables    = [];
    perturbations  = [];
    configurations = [];
    tokens         = [];
    volumes        = []
  }

(*
  let reverse res =
  let l_pat = List.rev !res.patterns
  and l_sig = List.rev !res.signatures
  and l_rul = List.rev !res.rules
  and l_ini = List.rev !res.init
  and l_obs = List.rev !res.observables
  in
  res:={patterns=l_pat ; signatures=l_sig ;
        rules=l_rul ; init = l_ini ; observables = l_obs}
*)

let print_link pr_port pr_type pr_annot f = function
  | FREE -> ()
  | LNK_TYPE (p, a) -> Format.fprintf f "!%a.%a" (pr_port a) p pr_type a
  | LNK_ANY -> Format.fprintf f "?"
  | LNK_SOME -> Format.fprintf f "!_"
  | LNK_VALUE (i,a) -> Format.fprintf f "!%i%a" i pr_annot a

let link_to_json port_to_json type_to_json annot_to_json = function
  | FREE -> `String "FREE"
  | LNK_TYPE (p, a) -> `List [port_to_json a p; type_to_json a]
  | LNK_ANY -> `Null
  | LNK_SOME -> `String "SOME"
  | LNK_VALUE (i,a) -> `List (`Int i :: annot_to_json a)
let link_of_json port_of_json type_of_json annot_of_json = function
  | `String "FREE" -> FREE
  | `List [p; a] -> let x = type_of_json a in LNK_TYPE (port_of_json x p, x)
  | `Null -> LNK_ANY
  | `String "SOME" -> LNK_SOME
  | `List (`Int i :: ( [] | _::_::_ as a)) -> LNK_VALUE (i,annot_of_json a)
  | x -> raise (Yojson.Basic.Util.Type_error ("Uncorrect link",x))

let print_ast_link =
  print_link
    (fun _ f (x,_) -> Format.pp_print_string f x)
    (fun f (x,_) -> Format.pp_print_string f x)
    (fun _ () -> ())
let print_ast_internal f l =
  Pp.list Pp.empty (fun f (x,_) -> Format.fprintf f "~%s" x) f l

let print_ast_port f p =
  Format.fprintf f "%s%a%a" (fst p.port_nme)
    print_ast_internal p.port_int
    (Pp.list Pp.empty (fun f (x,_) -> print_ast_link f x)) p.port_lnk

let string_annot_to_json = Location.annot_to_json JsonUtil.of_string
let string_annot_of_json =
  Location.annot_of_json (JsonUtil.to_string ?error_msg:None)

let port_to_json p =
  `Assoc [
    "port_nme", string_annot_to_json p.port_nme;
    "port_int", JsonUtil.of_list string_annot_to_json p.port_int;
    "port_lnk",
    JsonUtil.of_list
      (Location.annot_to_json
         (link_to_json
            (fun _ -> string_annot_to_json) string_annot_to_json (fun ()->[])))
      p.port_lnk;
  ]
let port_of_json = function
  | `Assoc [ "port_nme", n; "port_int", i; "port_lnk", l ] |
    `Assoc [ "port_nme", n; "port_lnk", l; "port_int", i ] |
    `Assoc [ "port_int", i; "port_nme", n; "port_lnk", l ] |
    `Assoc [ "port_lnk", l; "port_nme", n; "port_int", i ] |
    `Assoc [ "port_int", i; "port_lnk", l; "port_nme", n ] |
    `Assoc [ "port_lnk", l; "port_int", i; "port_nme", n ] ->
    { port_nme = string_annot_of_json n;
     port_int = JsonUtil.to_list string_annot_of_json i;
      port_lnk =
        JsonUtil.to_list
          (Location.annot_of_json
             (link_of_json
                (fun _ -> string_annot_of_json) string_annot_of_json
                (fun _ -> ()))) l;}
  | x -> raise (Yojson.Basic.Util.Type_error ("Not an AST agent",x))

let print_ast_agent f ((ag_na,_),l) =
  Format.fprintf f "%s(%a)" ag_na
    (Pp.list (fun f -> Format.fprintf f ",") print_ast_port) l

let agent_to_json (na,l) =
  `Assoc [ "name", Location.annot_to_json JsonUtil.of_string na;
         "sig", JsonUtil.of_list port_to_json l]
let agent_of_json = function
  | `Assoc [ "name", n; "sig", s ] | `Assoc [ "sig", s; "name", n ] ->
    (Location.annot_of_json (JsonUtil.to_string ?error_msg:None) n,
    JsonUtil.to_list port_of_json s)
  | x -> raise (Yojson.Basic.Util.Type_error ("Not an AST agent",x))

let print_ast_mix f m = Pp.list Pp.comma print_ast_agent f m

let init_to_json f_mix f_var = function
  | INIT_MIX m -> `List [`String "mixture"; f_mix m ]
  | INIT_TOK t -> `List [`String "token"; f_var t ]

let init_of_json f_mix f_var = function
  | `List [`String "mixture"; m ] -> INIT_MIX (f_mix m)
  | `List [`String "token"; t ] -> INIT_TOK (f_var t)
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid Ast init statement",x))

let print_tok pr_mix pr_tok pr_var f ((nb,_),(n,_)) =
  Format.fprintf f "%a:%a" (Alg_expr.print pr_mix pr_tok pr_var) nb pr_tok n
let print_one_size tk f mix =
  Format.fprintf
    f "%a%t%a" print_ast_mix mix
    (fun f -> match tk with [] -> () | _::_ -> Format.pp_print_string f " | ")
    (Pp.list
       (fun f -> Format.pp_print_string f " + ")
       (print_tok
          (fun f m -> Format.fprintf f "|%a|" print_ast_mix m)
          Format.pp_print_string (fun f x -> Format.fprintf f "'%s'" x)))
    tk
let print_arrow f bidir =
  Format.pp_print_string f (if bidir then "<->" else "->")

let print_raw_rate pr_mix pr_tok pr_var op f (def,_) =
  Format.fprintf
    f "%a%t" (Alg_expr.print pr_mix pr_tok pr_var) def
    (fun f ->
       match op with
         None -> ()
       | Some (d,_) ->
         Format.fprintf f ", %a" (Alg_expr.print pr_mix pr_tok pr_var) d)
let print_rates un op f def =
  Format.fprintf
    f "%a%t"
    (print_raw_rate
       (fun f m -> Format.fprintf f "|%a|" print_ast_mix m)
       Format.pp_print_string (fun f x -> Format.fprintf f "'%s'" x) op)
    def
    (fun f ->
       match un with
         None -> ()
       | Some ((d,_),max_dist) ->
         Format.fprintf
           f "(%a:%a)"
           (Alg_expr.print
              (fun f m -> Format.fprintf f "|%a|" print_ast_mix m)
              Format.pp_print_string (fun f x -> Format.fprintf f "'%s'" x)) d
           (Pp.option (fun f (md,_) ->
                        Format.fprintf f ":%a"
                        (Alg_expr.print
                           (fun f m -> Format.fprintf f "|%a|" print_ast_mix m)
                           Format.pp_print_string
                           (fun f x -> Format.fprintf f "'%s'" x)) md))
           max_dist)

let print_ast_rule f r =
  Format.fprintf
    f "@[<h>%a %a %a @@ %a@]"
    (print_one_size r.rm_token) r.lhs
    print_arrow r.bidirectional
    (print_one_size r.add_token) r.rhs
    (print_rates r.k_un r.k_op) r.k_def
let print_ast_rule_no_rate ~reverse f r =
  Format.fprintf
    f "@[<h>%a -> %a@]"
    (print_one_size r.rm_token) (if reverse then r.rhs else r.lhs)
    (print_one_size r.add_token) (if reverse then r.lhs else r.rhs)

let rule_to_json f_mix f_var r =
  `Assoc [
    "lhs", f_mix r.lhs;
    "rm_token",
    JsonUtil.of_list
      (JsonUtil.of_pair
         (Location.annot_to_json (Alg_expr.e_to_yojson f_mix f_var))
         string_annot_to_json)
      r.rm_token;
    "bidirectional", `Bool r.bidirectional;
    "rhs", f_mix r.rhs;
    "add_token",
    JsonUtil.of_list
      (JsonUtil.of_pair
         (Location.annot_to_json (Alg_expr.e_to_yojson f_mix f_var))
         string_annot_to_json)
      r.add_token;
    "k_def", Location.annot_to_json (Alg_expr.e_to_yojson f_mix f_var) r.k_def;
    "k_un",
    JsonUtil.of_option
      (JsonUtil.of_pair
         (Location.annot_to_json (Alg_expr.e_to_yojson f_mix f_var))
         (JsonUtil.of_option (Location.annot_to_json
                                (Alg_expr.e_to_yojson f_mix f_var))))
      r.k_un;
    "k_op",
    JsonUtil.of_option
      (Location.annot_to_json (Alg_expr.e_to_yojson f_mix f_var)) r.k_op;
    "k_op_un",
    JsonUtil.of_option
      (JsonUtil.of_pair
         (Location.annot_to_json (Alg_expr.e_to_yojson f_mix f_var))
         (JsonUtil.of_option (Location.annot_to_json
                                (Alg_expr.e_to_yojson f_mix f_var))))
      r.k_op_un;
  ]

let rule_of_json f_mix f_var = function
  | `Assoc l as x when List.length l < 9 ->
    begin
      try
        {
          lhs = f_mix (List.assoc "lhs" l);
          rm_token =
            JsonUtil.to_list
              (JsonUtil.to_pair
                 (Location.annot_of_json (Alg_expr.e_of_yojson f_mix f_var))
                 string_annot_of_json)
              (List.assoc "rm_token" l);
          bidirectional =
            Yojson.Basic.Util.to_bool (List.assoc "bidirectional" l);
          rhs = f_mix (List.assoc "rhs" l);
          add_token =
            JsonUtil.to_list
              (JsonUtil.to_pair
                 (Location.annot_of_json (Alg_expr.e_of_yojson f_mix f_var))
                 string_annot_of_json)
              (List.assoc "add_token" l);
          k_def = Location.annot_of_json
              (Alg_expr.e_of_yojson f_mix f_var) (List.assoc "k_def" l);
          k_un =
            JsonUtil.to_option
              (JsonUtil.to_pair
                 (Location.annot_of_json (Alg_expr.e_of_yojson f_mix f_var))
                 (JsonUtil.to_option
                    (Location.annot_of_json (Alg_expr.e_of_yojson f_mix f_var))))
              (List.assoc "k_un" l);
          k_op = JsonUtil.to_option
              (Location.annot_of_json (Alg_expr.e_of_yojson f_mix f_var))
              (List.assoc "k_op" l);
          k_op_un =
            JsonUtil.to_option
              (JsonUtil.to_pair
                 (Location.annot_of_json (Alg_expr.e_of_yojson f_mix f_var))
                 (JsonUtil.to_option
                    (Location.annot_of_json (Alg_expr.e_of_yojson f_mix f_var))))
              (List.assoc "k_op_un" l);
        }
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Incorrect AST rule",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect AST rule",x))

let print_expr_to_json f_mix f_var = function
  | Str_pexpr s -> string_annot_to_json s
  | Alg_pexpr a -> Location.annot_to_json (Alg_expr.e_to_yojson f_mix f_var) a

let print_expr_of_json f_mix f_var x =
  try Str_pexpr (string_annot_of_json x)
  with Yojson.Basic.Util.Type_error _ ->
  try Alg_pexpr (Location.annot_of_json (Alg_expr.e_of_yojson f_mix f_var) x)
  with Yojson.Basic.Util.Type_error _ ->
    raise (Yojson.Basic.Util.Type_error ("Incorrect print expr",x))

let modif_to_json f_mix f_var = function
  | INTRO (alg,mix) ->
    `List [ `String "INTRO";
            Location.annot_to_json (Alg_expr.e_to_yojson f_mix f_var) alg;
            Location.annot_to_json f_mix mix ]
  | DELETE (alg,mix) ->
    `List [ `String "DELETE";
            Location.annot_to_json (Alg_expr.e_to_yojson f_mix f_var) alg;
            Location.annot_to_json f_mix mix ]
  | UPDATE (id,alg) ->
    `List [ `String "UPDATE";
            Location.annot_to_json f_var id;
            Location.annot_to_json (Alg_expr.e_to_yojson f_mix f_var) alg ]
  | UPDATE_TOK (id,alg) ->
    `List [ `String "UPDATE_TOK";
            Location.annot_to_json f_var id;
            Location.annot_to_json (Alg_expr.e_to_yojson f_mix f_var) alg ]
  | STOP l ->
    `List (`String "STOP" :: List.map (print_expr_to_json f_mix f_var) l)
  | SNAPSHOT l ->
    `List (`String "SNAPSHOT" :: List.map (print_expr_to_json f_mix f_var) l)
  | PRINT (file,expr) ->
    `List [ `String "PRINT";
            JsonUtil.of_list (print_expr_to_json f_mix f_var) file;
            JsonUtil.of_list (print_expr_to_json f_mix f_var) expr ]
  | PLOTENTRY -> `String "PLOTENTRY"
  | CFLOWLABEL (b,id) ->
    `List [ `String "CFLOWLABEL"; `Bool b; string_annot_to_json id ]
  | CFLOWMIX (b,m) ->
    `List [ `String "CFLOW"; `Bool b; Location.annot_to_json f_mix m ]
  | FLUX (b,file) ->
    `List [ `String "FLUX"; `Bool b;
            JsonUtil.of_list (print_expr_to_json f_mix f_var) file ]
  | FLUXOFF file ->
    `List (`String "FLUXOFF" :: List.map (print_expr_to_json f_mix f_var) file)

let modif_of_json f_mix f_var = function
  | `List [ `String "INTRO"; alg; mix ] ->
     INTRO
       (Location.annot_of_json (Alg_expr.e_of_yojson f_mix f_var) alg,
        Location.annot_of_json f_mix mix)
  | `List [ `String "DELETE"; alg; mix ] ->
    DELETE
      (Location.annot_of_json (Alg_expr.e_of_yojson f_mix f_var) alg,
       Location.annot_of_json f_mix mix)
  | `List [ `String "UPDATE"; id; alg ] ->
    UPDATE
      (Location.annot_of_json f_var id,
       Location.annot_of_json (Alg_expr.e_of_yojson f_mix f_var) alg)
  | `List [ `String "UPDATE_TOK"; id; alg ] ->
     UPDATE_TOK
       (Location.annot_of_json f_var id,
        Location.annot_of_json (Alg_expr.e_of_yojson f_mix f_var) alg)
  | `List (`String "STOP" :: l) ->
    STOP (List.map (print_expr_of_json f_mix f_var) l)
  | `List (`String "SNAPSHOT" :: l) ->
    SNAPSHOT (List.map (print_expr_of_json f_mix f_var) l)
  | `List [ `String "PRINT"; file; expr ] ->
     PRINT
       (JsonUtil.to_list (print_expr_of_json f_mix f_var) file,
        JsonUtil.to_list (print_expr_of_json f_mix f_var) expr)
  | `String "PLOTENTRY" -> PLOTENTRY
  | `List [ `String "CFLOWLABEL"; `Bool b; id ] ->
     CFLOWLABEL (b, string_annot_of_json id)
  | `List [ `String "CFLOW"; `Bool b; m ] ->
     CFLOWMIX (b, Location.annot_of_json f_mix m)
  | `List [ `String "FLUX"; `Bool b; file ] ->
     FLUX (b, JsonUtil.to_list (print_expr_of_json f_mix f_var) file)
  | `List (`String "FLUXOFF" :: file) ->
     FLUXOFF (List.map (print_expr_of_json f_mix f_var) file)
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid modification",x))

let merge_internals =
  List.fold_left
    (fun acc (x,_ as y) ->
       if List.exists (fun (x',_) -> String.compare x x' = 0) acc
       then acc else y::acc)

let merge_ports =
  List.fold_left
    (fun acc p ->
       let rec aux = function
         | [] -> [{p with port_lnk = []}]
         | h :: t when fst p.port_nme = fst h.port_nme ->
           {h with port_int = merge_internals h.port_int p.port_int}::t
         | h :: t -> h :: aux t in
       aux acc)

let merge_agents =
  List.fold_left
    (fun acc ((na,_ as x),s) ->
       let rec aux = function
         | [] -> [x,List.map
                    (fun p -> {p with port_lnk = []}) s]
         | ((na',_),s') :: t when String.compare na na' = 0 ->
           (x,merge_ports s' s)::t
         | h :: t -> h :: aux t in
       aux acc)

let merge_tokens =
  List.fold_left
    (fun acc (_,(na,_ as tok)) ->
       let rec aux = function
         | [] -> [ tok ]
         | (na',_) :: _ as l when String.compare na na' = 0 -> l
         | h :: t as l ->
           let o = aux t in
           if t == o then l else h::o in
       aux acc)

let sig_from_inits =
  List.fold_left
    (fun (ags,toks) -> function
       | _,_,(INIT_MIX m,_) -> (merge_agents ags m,toks)
       | _,na,(INIT_TOK t,pos) -> (ags,merge_tokens toks [na,(t,pos)]))

let sig_from_rules =
  List.fold_left
    (fun (ags,toks) (_,(r,_)) ->
       let (ags',toks') =
         if r.bidirectional then
           (merge_agents ags r.rhs, merge_tokens toks r.add_token)
       else (ags,toks) in
       (merge_agents ags' r.lhs, merge_tokens toks' r.rm_token))

let sig_from_perts =
  List.fold_left
    (fun acc ((_,p,_),_) ->
       List.fold_left
         (fun (ags,toks) -> function
            | INTRO (_,(m,_)) ->
              (merge_agents ags m,toks)
            | UPDATE_TOK (t,na) ->
              (ags,merge_tokens toks [na,t])
            | (DELETE _ | UPDATE _ | STOP _ | SNAPSHOT _ | PRINT _ | PLOTENTRY |
               CFLOWLABEL _ | CFLOWMIX _ | FLUX _ | FLUXOFF _) -> (ags,toks))
         acc p)

let implicit_signature r =
  let acc = sig_from_inits (r.signatures,r.tokens) r.init in
  let acc' = sig_from_rules acc r.rules in
  let ags,toks = sig_from_perts acc' r.perturbations in
  { r with signatures = ags; tokens = toks }

let compil_to_json c =
    let mix_to_json = JsonUtil.of_list agent_to_json in
    let var_to_json = JsonUtil.of_string in
  `Assoc
    [
      "signatures", JsonUtil.of_list agent_to_json c.signatures;
      "tokens", JsonUtil.of_list string_annot_to_json c.tokens;
      "variables", JsonUtil.of_list
        (JsonUtil.of_pair
           string_annot_to_json
           (Location.annot_to_json
              (Alg_expr.e_to_yojson mix_to_json var_to_json)))
        c.variables;
      "rules", JsonUtil.of_list
        (JsonUtil.of_pair
           (JsonUtil.of_option string_annot_to_json)
           (Location.annot_to_json (rule_to_json mix_to_json var_to_json)))
        c.rules;
      "observables",
      JsonUtil.of_list
        (Location.annot_to_json (Alg_expr.e_to_yojson mix_to_json var_to_json))
        c.observables;
      "init",
      JsonUtil.of_list
        (JsonUtil.of_pair
           (Location.annot_to_json
              (Alg_expr.e_to_yojson mix_to_json var_to_json))
           (Location.annot_to_json (init_to_json mix_to_json var_to_json)))
        (List.map (fun (_,a,i) -> (a,i)) c.init);
      "perturbations", JsonUtil.of_list
        (Location.annot_to_json
           (fun (pre,modif,post) ->
              `List [
                Location.annot_to_json
                  (Alg_expr.bool_to_yojson mix_to_json var_to_json) pre;
                JsonUtil.of_list (modif_to_json mix_to_json var_to_json) modif;
                JsonUtil.of_option
                  (Location.annot_to_json
                     (Alg_expr.bool_to_yojson mix_to_json var_to_json)) post;
              ])) c.perturbations;
      "configurations",
      JsonUtil.of_list
        (JsonUtil.of_pair
           string_annot_to_json (JsonUtil.of_list string_annot_to_json))
        c.configurations;
    ]

let compil_of_json = function
  | `Assoc l as x when List.length l = 8 ->
    let mix_of_json = JsonUtil.to_list agent_of_json in
    let var_of_json = JsonUtil.to_string ?error_msg:None in
    begin
      try
        {
          signatures =
            JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST signature")
              agent_of_json (List.assoc "signatures" l);
          tokens =
            JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST token sig")
              string_annot_of_json (List.assoc "tokens" l);
          variables =
            JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST variables")
              (JsonUtil.to_pair
                 string_annot_of_json
                 (Location.annot_of_json
                    (Alg_expr.e_of_yojson mix_of_json var_of_json)))
              (List.assoc "variables" l);
          rules =
            JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST rules")
              (JsonUtil.to_pair
                 (JsonUtil.to_option string_annot_of_json)
                 (Location.annot_of_json
                    (rule_of_json mix_of_json var_of_json)))
              (List.assoc "rules" l);
          observables =
            JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST observables")
              (Location.annot_of_json
                 (Alg_expr.e_of_yojson mix_of_json var_of_json))
              (List.assoc "observables" l);
          init =
            List.map
              (fun (a,i) -> (None,a,i))
              (JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST init")
                 (JsonUtil.to_pair
                    (Location.annot_of_json
                       (Alg_expr.e_of_yojson mix_of_json var_of_json))
                    (Location.annot_of_json
                       (init_of_json mix_of_json var_of_json)))
                 (List.assoc "init" l));
          perturbations =
            JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST perturbations")
              (Location.annot_of_json
                 (function
                   | `List [pre; modif; post] ->
                     (Location.annot_of_json
                        (Alg_expr.bool_of_yojson mix_of_json var_of_json)
                        pre,
                      JsonUtil.to_list
                        (modif_of_json mix_of_json var_of_json) modif,
                      JsonUtil.to_option
                        (Location.annot_of_json
                           (Alg_expr.bool_of_yojson mix_of_json var_of_json))
                        post)
                   | x ->
                     raise
                       (Yojson.Basic.Util.Type_error ("Not a perturbation",x))
                 ))
              (List.assoc "perturbations" l);
          configurations =
            JsonUtil.to_list ~error_msg:(JsonUtil.build_msg "AST configuration")
              (JsonUtil.to_pair
                 string_annot_of_json (JsonUtil.to_list string_annot_of_json))
              (List.assoc "configurations" l);
          volumes = [];
        }
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Incorrect AST",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Incorrect AST",x))
