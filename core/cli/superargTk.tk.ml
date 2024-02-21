(* LablTK GUI for option selection Superarg.

   Copyright (C) Antoine Mine' 2006
*)

open Tk
module StringSetMap = Mods.StringSetMap
module StringMap = StringSetMap.Map

let map = ref StringMap.empty (* key => entry widget *)
let fmap = ref StringMap.empty (* key => frame widget *)

(* show / hide options according to current mode *)
let set_visibility (a : Superarg.t) =
  List.iter
    (fun (key, _, _, _, lvl) ->
      try
        let f =
          match StringMap.find_option key !fmap with
          | Some f -> f
          | None -> raise Not_found
        in
        List.iter (fun f -> Pack.forget [ coe f ]) f;
        if Superarg.show_level lvl then
          List.iter (fun f -> pack ~side:`Top ~anchor:`W [ coe f ]) f
      with Not_found -> ())
    a

let set_visibility_bis a =
  List.iter (fun (f, _) -> Pack.forget [ coe f ]) a;
  List.iter
    (fun (f, lvl) ->
      if Superarg.show_level lvl then pack ~side:`Left ~padx:1 ~pady:1 [ coe f ])
    a

let set_visibility (a, b) =
  set_visibility a;
  set_visibility_bis b

let plist_to_list l =
  List.fold_left (fun l (a, b) -> a :: b :: l) [] (List.rev l)

(* option value => widget value *)
let widget_update_from_spec (a : Superarg.t) =
  let set n v =
    try
      Textvariable.set
        (match StringMap.find_option n !map with
        | None -> raise Not_found
        | Some a -> a)
        v
    with Not_found -> ()
  in
  List.iter
    (fun (key, spec, _msg, _cat, _lvl) ->
      match spec with
      | Superarg.Void -> ()
      | Superarg.Bool r ->
        set key
          (if !r then
             "1"
           else
             "0")
      | Superarg.Int r -> set key (string_of_int !r)
      | Superarg.Int_opt r ->
        set key
          (match !r with
          | None -> ""
          | Some i -> string_of_int i)
      | Superarg.String r -> set key !r
      | Superarg.String_opt r ->
        set key
          (match !r with
          | None -> ""
          | Some s -> s)
      | Superarg.String_list r -> set key (String.concat " " !r)
      | Superarg.StringNbr_list r ->
        set key (String.concat " " (plist_to_list !r))
      | Superarg.Float r -> set key (string_of_float !r)
      | Superarg.Float_opt r ->
        set key
          (match !r with
          | None -> ""
          | Some f -> string_of_float f)
      | Superarg.Choice (_, _, r) -> set key !r
      | Superarg.Choice_list (l, r) ->
        List.iter
          (fun (k, _) ->
            set
              (key ^ "." ^ k)
              (if List.mem k !r then
                 "1"
               else
                 "0"))
          l
      | Superarg.Multi (_, []) -> ()
      | Superarg.Multi (_, _) -> set key ""
      | Superarg.MultiExt _ -> set key "")
    a

(* command-line argument => widget value *)
let widget_update_from_cmd (a : Superarg.t) l =
  let rec doit accum = function
    | [] -> accum
    | ("-help" | "--help" | "-h" | "--gui") :: rem -> doit accum rem
    | "--expert" :: rem ->
      Superarg.expert_mode := true;
      set_visibility (a, []);
      doit accum rem
    | "--no-expert" :: rem ->
      Superarg.expert_mode := false;
      set_visibility (a, []);
      doit accum rem
    | opt :: rem ->
      if opt = "-" then
        List.rev_append rem accum
      else (
        try
          let key, spec, _, _, _ =
            List.find
              (fun (key, _, _, _, _) -> opt = key || opt = Superarg.nokey key)
              a
          in
          let set n v =
            Textvariable.set
              (match StringMap.find_option n !map with
              | Some a -> a
              | None -> raise Not_found)
              v
          and get n =
            Textvariable.get
              (match StringMap.find_option n !map with
              | Some a -> a
              | None -> raise Not_found)
          in
          let rem =
            match spec, rem with
            | Superarg.Void, rem -> rem
            | Superarg.Bool _, rem ->
              set key
                (if opt = key then
                   "1"
                 else
                   "0");
              rem
            | ( ( Superarg.Int _ | Superarg.Int_opt _ | Superarg.String _
                | Superarg.String_opt _ | Superarg.Float _
                | Superarg.Float_opt _ | Superarg.Choice _ ),
                v :: rem )
              when opt = key ->
              set key v;
              rem
            | (Superarg.String_list _ | Superarg.StringNbr_list _), v :: rem
              when opt = key ->
              set key (get key ^ " " ^ v);
              rem
            | ( ( Superarg.Int _ | Superarg.Int_opt _ | Superarg.String _
                | Superarg.String_opt _ | Superarg.Float _
                | Superarg.Float_opt _ | Superarg.Choice _
                | Superarg.String_list _ | Superarg.StringNbr_list _ ),
                rem ) ->
              set key "";
              rem
            | Superarg.Choice_list _, v :: rem when opt = key ->
              set (key ^ "." ^ v) "1";
              rem
            | Superarg.Choice_list (l, _), rem ->
              List.iter (fun (v, _) -> set (key ^ "." ^ v) "0") l;
              rem
            | Superarg.Multi (x, []), rem ->
              ignore (doit [] x);
              rem
            | Superarg.Multi (x, y), v :: rem ->
              set key v;
              if v <> "" then (
                ignore (doit [] x);
                ignore
                  (List.fold_left (fun _accum l -> doit [] [ l; v ]) accum y)
              );
              rem
            | Superarg.MultiExt l, v :: rem ->
              set key v;
              if v <> "" then
                ignore
                  (List.fold_left
                     (fun _accum (l, ext) -> doit [] [ l; v ^ ext ])
                     accum l);
              rem
            | _, [] -> rem
          in
          doit accum rem (* option eaten *)
        with _ -> doit (opt :: accum) rem (* option accumulated *)
      )
  in
  List.rev (doit [] l)

(* widget value => command-line argument,
   if [short]=[true] no command is output is the value is the default one *)
let cmd_of_widget (a : Superarg.t) short =
  let get n =
    Textvariable.get
      (match StringMap.find_option n !map with
      | Some a -> a
      | None -> raise Not_found)
  in
  List.fold_left
    (fun accum (key, spec, _msg, cat, _lvl) ->
      try
        match spec with
        | Superarg.Void -> accum
        | Superarg.Bool r ->
          let v = get key = "1" in
          if !r = v && short then
            accum
          else if v then
            key :: accum
          else
            Superarg.nokey key :: accum
        | Superarg.Int r ->
          let v = get key in
          if !r = int_of_string v && short then
            accum
          else
            key :: v :: accum
        | Superarg.Int_opt r ->
          let v = get key in
          if v = "" && (!r <> None || not short) then
            Superarg.nokey key :: accum
          else if v <> "" && (!r <> Some (int_of_string v) || not short) then
            key :: v :: accum
          else
            accum
        | Superarg.String r ->
          let v = get key in
          if v = "" && (!r <> "" || not short) then
            Superarg.nokey key :: accum
          else if v <> "" && (!r <> v || not short) then
            key :: v :: accum
          else
            accum
        | Superarg.String_opt r ->
          let v = get key in
          if v = "" && (!r <> None || not short) then
            Superarg.nokey key :: accum
          else if v <> "" && (!r <> Some v || not short) then
            key :: v :: accum
          else
            accum
        | Superarg.String_list r ->
          let v = Superarg.cut_list (get key) in
          if v = !r && short then
            accum
          else if v = [] then
            Superarg.nokey key :: accum
          else
            List.fold_right (fun x accum -> key :: x :: accum) v accum
        | Superarg.StringNbr_list r ->
          let v = Superarg.cut_list (get key) in
          if v = plist_to_list !r && short then
            accum
          else if v = [] then
            Superarg.nokey key :: accum
          else
            List.fold_right (fun x accum -> key :: x :: accum) v accum
        | Superarg.Float r ->
          let v = get key in
          if !r = float_of_string v && short then
            accum
          else
            key :: v :: accum
        | Superarg.Float_opt r ->
          let v = get key in
          if v = "" && (!r <> None || not short) then
            Superarg.nokey key :: accum
          else if v <> "" && (!r <> Some (float_of_string v) || not short) then
            key :: v :: accum
          else
            accum
        | Superarg.Choice (ll, _, r) ->
          let v = get key in
          if not (List.exists (fun (k, _) -> v = k) ll) then
            failwith "invalid Superarg.Choice"
          else if !r = v && short then
            accum
          else
            key :: v :: accum
        | Superarg.Choice_list (ll, r) ->
          let v =
            List.fold_left
              (fun accum (k, _) ->
                try
                  if get (key ^ "." ^ k) = "1" then
                    k :: accum
                  else
                    accum
                with Not_found -> accum)
              [] ll
          in
          if !r = v && short then
            accum
          else if v = [] then
            Superarg.nokey key :: accum
          else
            List.fold_left (fun accum x -> key :: x :: accum) accum v
        | Superarg.Multi _ -> accum
        | Superarg.MultiExt _ -> accum
      with
      | Not_found -> accum
      | Failure f ->
        failwith
          ("Invalid argument type for option " ^ key ^ " in "
          ^ List.fold_left (fun sol ((x, _, _), _) -> sol ^ " " ^ x) "" cat
          ^ ": " ^ f))
    [] (List.rev a)

let balloon_delay = 100

(* create an option widget, add the defined variables to the map *)
let widget_of_spec (a : Superarg.t) key spec msg lvl parent =
  let f = Frame.create parent in
  let old = StringMap.find_default [] key !fmap in
  let fmap' = StringMap.add key (f :: old) !fmap in
  let _ = fmap := fmap' in
  let v =
    match StringMap.find_option key !map with
    | Some a -> a
    | None -> Textvariable.create ()
  in
  (match spec with
  | Superarg.Bool _ ->
    let chk = Checkbutton.create ~variable:v ~text:key f in
    pack ~side:`Left ~anchor:`W [ chk ];
    Balloon.put ~on:(coe chk) ~ms:balloon_delay msg;
    map := StringMap.add key v !map
  | Superarg.Void ->
    let lbl = Label.create ~text:" " ~padx:20 f in
    pack ~side:`Left ~expand:true ~fill:`X ~anchor:`W [ coe lbl ];
    Balloon.put ~on:(coe lbl) ~ms:balloon_delay msg
  | Superarg.Int _ | Superarg.Int_opt _ | Superarg.String _
  | Superarg.String_opt _ | Superarg.String_list _ | Superarg.StringNbr_list _
  | Superarg.Float _ | Superarg.Float_opt _ ->
    let ext =
      match spec with
      | Superarg.Int _ | Superarg.Int_opt _ -> "<int>"
      | Superarg.String _ | Superarg.String_opt _ -> "<name>"
      | Superarg.String_list _ | Superarg.StringNbr_list _ -> "<names> ..."
      | Superarg.Float _ | Superarg.Float_opt _ -> "<float>"
      | Superarg.Void | Superarg.Bool _ | Superarg.Choice _
      | Superarg.Choice_list _ | Superarg.Multi _ | Superarg.MultiExt _ ->
        ""
    in
    let lbl = Label.create ~text:(key ^ " " ^ ext) ~padx:20 f
    and entry = Entry.create ~textvariable:v f in
    pack ~side:`Left ~expand:true ~fill:`X ~anchor:`W [ coe lbl; coe entry ];
    Balloon.put ~on:(coe lbl) ~ms:balloon_delay msg;
    Balloon.put ~on:(coe entry) ~ms:balloon_delay msg;
    map := (*snd*) StringMap.add key v !map
  | Superarg.Choice (l, _, _) ->
    let lbl = Label.create ~text:key ~padx:20 f in
    let fff = Frame.create f in
    let ff = Frame.create fff in
    let p = ref 0 in
    List.iter
      (fun (k, msg2) ->
        let radio =
          Radiobutton.create ~variable:v ~text:k ~value:k ~padx:40 ff
        in
        Grid.configure ~sticky:"w" ~column:(!p mod 3) ~row:(!p / 3) [ radio ];
        Balloon.put ~on:(coe radio) ~ms:balloon_delay (msg ^ ":\n" ^ msg2);
        incr p)
      l;
    pack ~side:`Top ~anchor:`W [ coe lbl; coe ff ];
    pack ~side:`Left ~anchor:`W [ coe fff ];
    Balloon.put ~on:(coe lbl) ~ms:balloon_delay msg;
    map := StringMap.add key v !map
  | Superarg.Choice_list (l, _) ->
    let lbl = Label.create ~text:key ~padx:20 f in
    let fff = Frame.create f in
    let ff = Frame.create fff in
    let p = ref 0 in
    let nb = 4 in
    (* number of columns *)
    List.iter
      (fun (k, msg2) ->
        let v = Textvariable.create () in
        let chk = Checkbutton.create ~variable:v ~text:k ~padx:40 ff in
        Grid.configure ~sticky:"w" ~column:(!p mod nb) ~row:(!p / nb) [ chk ];
        incr p;
        Balloon.put ~on:(coe chk) ~ms:balloon_delay (msg ^ ":\n" ^ msg2);
        map := StringMap.add (key ^ "." ^ k) v !map)
      l;
    pack ~side:`Top ~anchor:`W [ coe lbl; coe ff ];
    pack ~side:`Left ~anchor:`W [ coe fff ];
    Balloon.put ~on:(coe lbl) ~ms:balloon_delay msg
  | Superarg.Multi (x, []) ->
    let update () = ignore (widget_update_from_cmd a x) in
    let b = Button.create ~text:key ~padx:20 ~command:update f in
    pack ~side:`Left ~anchor:`W [ coe b ];
    let msg2 = msg ^ "\n(equivalent to " ^ String.concat " " x ^ " )" in
    Balloon.put ~on:(coe b) ~ms:balloon_delay msg2;
    map := StringMap.add key v !map
  | Superarg.Multi (x, y) ->
    let rec update () =
      let s = Textvariable.get v in
      if s <> "" then (
        ignore (widget_update_from_cmd a x);
        List.iter (fun o -> ignore (widget_update_from_cmd a [ o; s ])) y
      );
      Textvariable.handle v ~callback:update
    in
    let lbl = Label.create ~text:key ~padx:20 f
    and entry = Entry.create ~textvariable:v f in
    pack ~side:`Left ~expand:true ~fill:`X ~anchor:`W [ coe lbl; coe entry ];
    let msg2 = msg ^ "\n(equivalent to " ^ String.concat " " (x @ y) ^ " )" in
    Balloon.put ~on:(coe lbl) ~ms:balloon_delay msg2;
    Balloon.put ~on:(coe entry) ~ms:balloon_delay msg2;
    Textvariable.handle v ~callback:update;
    map := StringMap.add key v !map
  | Superarg.MultiExt l ->
    let rec update () =
      let s = Textvariable.get v in
      if s <> "" then
        List.iter
          (fun (o, ext) -> ignore (widget_update_from_cmd a [ o; s ^ ext ]))
          l;
      Textvariable.handle v ~callback:update
    in
    let lbl = Label.create ~text:key ~padx:20 f
    and entry = Entry.create ~textvariable:v f in
    pack ~side:`Left ~expand:true ~fill:`X ~anchor:`W [ coe lbl; coe entry ];
    let msg2 =
      msg ^ "\n(equivalent to "
      ^ String.concat " " (List.rev_map snd (List.rev l))
      ^ " )"
    in
    Balloon.put ~on:(coe lbl) ~ms:balloon_delay msg2;
    Balloon.put ~on:(coe entry) ~ms:balloon_delay msg2;
    Textvariable.handle v ~callback:update;
    map := StringMap.add key v !map);

  let text =
    match lvl with
    | Superarg.Expert -> "(expert)"
    | Superarg.Developper -> "(developper)"
    | Superarg.Normal | Superarg.Hidden -> ""
  in
  if text <> "" then (
    let l = Label.create ~text f in
    Balloon.put ~on:(coe l) ~ms:balloon_delay msg;
    pack ~side:`Left ~anchor:`W [ l ]
  );
  pack ~side:`Top ~anchor:`W [ coe f ]

(* notebook *)
class pager bparent fparent =
  let bars = Frame.create bparent in
  (* Superarg.Multi-line button bar *)
  let bar = ref (Frame.create bars) (* one button line *)
  and barsize = ref 0
  and maxbarwidth = 65 (* split bar at this column (in chars) *)
  and cont = Frame.create fparent (* current page *)
  and cur = ref "" (* current page name *)
  and pages = ref StringMap.empty (* all pages *)
  and pages_lvl = ref [] in

  object (self)
    initializer
      pack ~side:`Top [ coe !bar ];
      pack ~side:`Top [ coe bars ];
      pack ~side:`Top ~pady:5 ~fill:`Both ~expand:true [ coe cont ]

    (* sets the page currently viewed *)
    method set_page name =
      (try
         let fr, _, b =
           match StringMap.find_option !cur !pages with
           | None -> raise Not_found
           | Some a -> a
         in
         cur := "";
         Button.configure ~relief:`Raised b;
         Pack.forget [ coe fr ]
       with Not_found -> ());
      try
        let fr, _, b =
          match StringMap.find_option name !pages with
          | None -> raise Not_found
          | Some a -> a
        in
        cur := name;
        Button.configure ~relief:`Sunken b;
        pack ~expand:true ~fill:`Both ~anchor:`Center [ coe fr ]
      with Not_found -> ()

    (* gets a page (create if non existing) *)
    method get_page name (lvl : Superarg.level) : Widget.frame Widget.widget =
      try
        let _, p, _ =
          match StringMap.find_option name !pages with
          | Some a -> a
          | None -> raise Not_found
        in
        p
      with Not_found ->
        if
          !barsize / maxbarwidth
          <> (!barsize + String.length name) / maxbarwidth
        then (
          bar := Frame.create bars;
          pack ~side:`Top [ coe !bar ]
        );
        let fr = Frame.create ~borderwidth:2 ~relief:`Ridge cont
        and lbl =
          Button.create ~text:name ~command:(fun () -> self#set_page name) !bar
        in
        let p = Frame.create fr in
        pack ~side:`Left ~padx:1 ~pady:1 [ coe lbl ];
        Grid.column_configure ~minsize:600 (coe fr) 0;
        Grid.row_configure ~minsize:400 (coe fr) 0;
        Grid.configure ~column:0 ~row:0 [ coe p ];
        barsize := !barsize + String.length name;
        pages :=
          (*snd*)
          StringMap.add
            (*_map*)
            (*parameters error*)
            name (fr, p, lbl) !pages;
        pages_lvl := (lbl, lvl) :: !pages_lvl;
        if !cur = "" then self#set_page name;
        p

    method get_pages_lvl () = List.rev !pages_lvl
  end

let build_spec (a : Superarg.t) bparent fparent =
  let opts = new pager bparent fparent in
  Superarg.StringIntMap.iter (*_map*)
    (fun _ (l, cat_lvl) ->
      List.iter
        (fun ( (key : Superarg.key),
               (spec : Superarg.spec),
               (msg : Superarg.msg),
               (cat : (Superarg.category * Superarg.position) list),
               (lvl : Superarg.level) ) ->
          List.iter
            (fun (((cat : string), _, lvl_opt), _) ->
              let cat_lvl = Superarg.max_level_opt cat_lvl lvl_opt in
              widget_of_spec a key spec msg lvl (opts#get_page cat cat_lvl))
            cat)
        l)
    (Superarg.order a);
  let _ = widget_update_from_spec a in
  opts#get_pages_lvl ()

exception Exit of string list

(* main *)
let gui ?title (a : Superarg.t) (args : string list) : string list =
  let top = openTk () in
  let () =
    appname_set
      (match title with
      | None -> Version.version_kasa_full_name
      | Some x -> x)
  in
  Balloon.init ();
  (* option list *)
  let up = Frame.create top in
  let left = Frame.create up
  and right = Frame.create up
  and middle = Frame.create top in
  pack ~side:`Top [ up ];
  pack ~side:`Top ~expand:true ~fill:`Both [ middle ];
  pack ~side:`Left ~padx:20 [ left; right ];
  let pages_lvl = build_spec a right middle in
  (* expert mode *)
  let expyes =
    Radiobutton.create ~text:"Expert" ~value:"1"
      ~command:(fun () ->
        Superarg.expert_mode := true;
        set_visibility (a, pages_lvl))
      left
  and expno =
    Radiobutton.create ~text:"Normal" ~value:"0"
      ~command:(fun () ->
        Superarg.expert_mode := false;
        set_visibility (a, pages_lvl))
      left
  in
  pack ~side:`Top ~anchor:`W [ expno; expyes ];

  (* file list *)
  let v = Textvariable.create () in
  let eframe = Frame.create top in
  let lbl = Label.create ~text:"Filenames: " eframe
  and entry = Entry.create ~width:80 ~textvariable:v eframe
  and but1 =
    Button.create ~text:"Add"
      ~command:(fun _ ->
        Fileselect.f ~title:"Add filenames"
          ~action:(fun l ->
            Textvariable.set v (Textvariable.get v ^ " " ^ String.concat " " l))
          ~filter:"*.ka" ~file:"" ~multi:true ~sync:true)
      eframe
  and but2 =
    Button.create ~text:"Clear" ~command:(fun _ -> Textvariable.set v "") eframe
  in

  pack ~side:`Left ~expand:true ~fill:`X
    [ coe lbl; coe entry; coe but1; coe but2 ];
  pack ~side:`Top ~pady:10 ~expand:true ~fill:`Both [ coe eframe ];

  (* extract command-line from widget values *)
  let cmd () = cmd_of_widget a true @ Superarg.cut_list (Textvariable.get v) in

  (* backup save that ignores errors *)
  let backup name =
    try
      let f = open_out name in
      output_string f (String.concat " " (cmd ()));
      close_out f
    with _ -> ()
  in

  (* buttons *)
  let bframe = Frame.create top in
  (* button bar *)
  let do_launch = ref false in

  let quit = Button.create ~text:"Quit" ~command:(fun _ -> closeTk ()) bframe
  and reset =
    Button.create ~text:"Reset to default"
      ~command:(fun _ ->
        backup "autosave_pre_reset.options";
        widget_update_from_spec a)
      bframe
  and import =
    Button.create ~text:"Import options"
      ~command:(fun _ ->
        Fileselect.f ~title:"Merge options from file"
          ~action:(function
            | [ name ] ->
              (try
                 let f = open_in name in
                 let b = Buffer.create 128 in
                 (try
                    while true do
                      Buffer.add_string b (input_line f);
                      Buffer.add_char b ' '
                    done
                  with End_of_file -> ());
                 close_in f;
                 let x = Superarg.cut_list (Buffer.contents b) in
                 backup "autosave_pre_import.options";
                 let rem = widget_update_from_cmd a x in
                 Textvariable.set v
                   (Textvariable.get v ^ " " ^ String.concat " " rem)
               with exc ->
                 ignore
                   (Dialog.create ~parent:(coe top) ~title:"Cannot load!"
                      ~message:(Printexc.to_string exc) ~buttons:[ "Close" ] ()))
            | _ -> ())
          ~filter:"*.options" ~file:"default.options" ~multi:false ~sync:true)
      bframe
  and save =
    Button.create ~text:"Save options"
      ~command:(fun _ ->
        try
          let result = String.concat " " (cmd ()) in
          Fileselect.f ~title:"Save file"
            ~action:(function
              | [ name ] ->
                (try
                   let f = open_out name in
                   output_string f result;
                   close_out f
                 with exc ->
                   ignore
                     (Dialog.create ~parent:(coe top) ~title:"Cannot save!"
                        ~message:(Printexc.to_string exc) ~buttons:[ "Close" ]
                        ()))
              | _ -> ())
            ~filter:"*.options" ~file:"default.options" ~multi:false ~sync:true
        with exc ->
          ignore
            (Dialog.create ~parent:(coe top) ~title:"Cannot save!"
               ~message:(Printexc.to_string exc) ~buttons:[ "Close" ] ()))
      bframe
  and go =
    Button.create ~text:"Launch analyze"
      ~command:(fun _ ->
        try
          let _ = cmd () in
          do_launch := true;
          closeTk ()
        with exc ->
          ignore
            (Dialog.create ~parent:(coe top) ~title:"Cannot launch analysis!"
               ~message:(Printexc.to_string exc) ~buttons:[ "Close" ] ()))
      bframe
  in

  pack ~side:`Left ~padx:40 ~fill:`X ~expand:true
    [ quit; reset; import; save; go ];
  pack ~side:`Top ~fill:`Both ~expand:true [ bframe ];

  (* get command-line arguments *)
  let rem = widget_update_from_cmd a args in
  Textvariable.set v (Textvariable.get v ^ " " ^ String.concat " " rem);
  Radiobutton.select
    (if !Superarg.expert_mode then
       expyes
     else
       expno);
  set_visibility (a, pages_lvl);

  (* tk loop *)
  mainLoop ();

  (* back from gui *)
  if not !do_launch then (
    backup "autosave_pre_quit.options";
    exit 0
  );
  backup "autosave_pre_launch.options";
  Printf.printf "/* The GUI launches the analysis with the options:\n%s\n*/\n"
    (String.concat " " (cmd ()));
  flush stdout;
  Version.tk_is_initialized := true;
  Superarg.parse_list ~with_tk:true a (cmd ())

(* MAIN *)
(* **** *)

let parse ?title (a : Superarg.t) (def : string list ref) =
  Superarg.check a;
  Version.tk_is_initialized := true;
  let a = Superarg.order a in
  let a =
    Superarg.StringIntMap.fold
      (fun _ (list, _) list' ->
        List.fold_left (fun list' a -> a :: list') list' (List.rev list))
      a []
  in
  (* drop the first command-line argument: it is the executable name *)
  let args = List.tl (Array.to_list Sys.argv) in
  (* if no argument or "--gui" given, launch the gui, otherwise, parse args *)
  let rem =
    if
      args = [] || args = [ "--expert" ] || args = [ "--no-expert" ]
      || List.exists (( = ) "--gui") args
    then
      gui ?title a args
    else
      Superarg.parse_list ~with_tk:true ?title a args
  in
  if rem <> [] then def := rem
