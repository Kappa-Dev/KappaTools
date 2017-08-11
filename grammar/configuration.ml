(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type t = {
  dumpIfDeadlocked : bool;
  initial : float option;
  maxConsecutiveClash : int;
  syntaxVersion : Ast.syntax_version;
  outputFileName : string option;
  plotPeriod : Counter.period option;
  seed : int option;
  traceFileName : string option;
  deltaActivitiesFileName : string option;
}

let empty = {
  dumpIfDeadlocked = true;
  initial = None;
  maxConsecutiveClash = 2;
  syntaxVersion = Ast.V3;
  seed = None;
  traceFileName = None;
  plotPeriod = None;
  outputFileName = None;
  deltaActivitiesFileName = None;
}

let parse result =
  let get_value pos_p param value_list f =
    match value_list with
    | [v,pos] -> f v pos
    | _ ->
      raise
        (ExceptionDefn.Malformed_Decl
           ("Wrong number of arguments for parameter "^param,pos_p)) in
  let get_bool_value pos_p param value_list =
    get_value pos_p param value_list
      (fun value pos_v ->
         match value with
         | "true" | "yes" -> true
         | "false" | "no" -> false
         | _ as error ->
           raise
             (ExceptionDefn.Malformed_Decl
                ("Value "^error^" should be either \"yes\" or \"no\"", pos_v))
      ) in
  List.fold_left
    (fun (conf,progress,story_compression,formatCflow,cflowFile)
      ((param,pos_p),value_list) ->
      match param with
      | "displayCompression" ->
        let rec parse (a,b,c) l =
          match l with
          | ("strong",_)::tl -> parse (a,b,true) tl
          | ("weak",_)::tl -> parse (a,true,c) tl
          | ("none",_)::tl -> parse (true,b,c) tl
          | [] -> (conf,progress,(a,b,c),formatCflow,cflowFile)
          | (error,pos)::_ ->
            raise (ExceptionDefn.Malformed_Decl
                     ("Unkown value "^error^" for compression mode", pos))
        in
        parse story_compression value_list
      | "cflowFileName" ->
        get_value pos_p param value_list
          (fun x _ -> (conf,progress,story_compression,formatCflow,Some x))
      | "seed" ->
        get_value pos_p param value_list
          (fun s p ->
             try
               ({ conf with seed = Some (int_of_string s) },
                progress,story_compression,formatCflow,cflowFile)
             with Failure _ ->
               raise (ExceptionDefn.Malformed_Decl
                        ("Value "^s^" should be an integer", p)))
      | "T0" ->
        get_value pos_p param value_list
          (fun s p ->
             try
               ({ conf with initial = Some (float_of_string s) },
                progress,story_compression,formatCflow,cflowFile)
             with Failure _ ->
               raise (ExceptionDefn.Malformed_Decl
                        ("Value "^s^" should be a float", p)))
      | "plotPeriod" ->
        begin match value_list with
          | [s,p] ->
            (try
               ({conf with plotPeriod = Some (Counter.DT (float_of_string s))},
                progress,story_compression,formatCflow,cflowFile)
             with Failure _ ->
               raise (ExceptionDefn.Malformed_Decl
                        ("Value "^s^" should be a float", p)))
          | [s,sp;u,up] ->
            if u = "e" || u = "event" || u = "events" ||
               u = "Event" || u = "Events" then
              try
                ({conf with plotPeriod = Some (Counter.DE (int_of_string s))},
                 progress,story_compression,formatCflow,cflowFile)
              with Failure _ ->
                raise (ExceptionDefn.Malformed_Decl
                         ("Value "^s^" should be an integer", sp))
            else if u = "t.u." || u = "time units" || u = "Time units" ||
                    u = "time unit" || u = "Time unit" then
              try
                ({conf with plotPeriod = Some (Counter.DT (float_of_string s))},
                 progress,story_compression,formatCflow,cflowFile)
              with Failure _ ->
                raise (ExceptionDefn.Malformed_Decl
                         ("Value "^s^" should be a float", sp))
            else
              raise (ExceptionDefn.Malformed_Decl
                       ("Incorrect unit "^u, up))
          | _ ->
            raise
              (ExceptionDefn.Malformed_Decl
                 ("Wrong number of arguments for parameter "^param,pos_p))
        end
      | "outputFileName" ->
        get_value pos_p param value_list
          (fun s _ ->
               ({ conf with outputFileName = Some s },
                progress,story_compression,formatCflow,cflowFile))
      | "traceFileName" ->
        get_value pos_p param value_list
          (fun s _ ->
               ({ conf with traceFileName = Some s },
                progress,story_compression,formatCflow,cflowFile))
      | "deltaActivitiesFileName" ->
        get_value pos_p param value_list
          (fun s _ ->
               ({ conf with deltaActivitiesFileName = Some s },
                progress,story_compression,formatCflow,cflowFile))

      | "progressBarSize" ->
         (conf,{ progress with
                 Counter.progressSize = get_value pos_p param value_list
                     (fun v p ->
                        try int_of_string v
                        with Failure _ ->
                          raise (ExceptionDefn.Malformed_Decl
                                   ("Value "^v^" should be an integer", p)))
               },story_compression,formatCflow,cflowFile)
      | "progressBarSymbol" ->
         (conf,{ progress with
                 Counter.progressChar = get_value pos_p param value_list
                     (fun v p ->
                        try
                          String.unsafe_get v 0
                        with _ ->
                          raise (ExceptionDefn.Malformed_Decl
                                   ("Value "^v^" should be a character",p)))
               },story_compression,formatCflow,cflowFile)

      | "syntaxVersion" ->
        get_value pos_p param value_list
          (fun v p ->
             try
               ({ conf with syntaxVersion = match int_of_string v with
                    | 3 -> Ast.V3
                    | 4 -> Ast.V4
                    | _ -> raise Not_found},
                progress,story_compression,formatCflow,cflowFile)
             with _ ->
               raise (ExceptionDefn.Malformed_Decl
                        ("Value "^v^" should be either 3 or 4",p)))
      | "dumpIfDeadlocked" ->
        ({ conf with dumpIfDeadlocked = get_bool_value pos_p param value_list },
         progress,story_compression,formatCflow,cflowFile)
      | "maxConsecutiveClash" ->
        get_value pos_p param value_list
          (fun v p ->
             try
               ({ conf with maxConsecutiveClash = int_of_string v },
                progress,story_compression,formatCflow,cflowFile)
             with _ ->
               raise (ExceptionDefn.Malformed_Decl
                        ("Value "^v^" should be an integer",p)))
      | "dotCflows" ->
         let formatCflow = get_value pos_p param value_list (fun v _ -> v) in
         (conf,progress,story_compression,formatCflow,cflowFile)
(*         if get_bool_value pos_p param value_list then
           (story_compression, Dot) else
           (story_compression, Html)*)
      | _ as error ->
        raise (ExceptionDefn.Malformed_Decl ("Unkown parameter "^error, pos_p))
    ) (empty, Counter.default_progress, (false,false,false), "dot", None) result

let print f conf =
  let () = Format.pp_open_vbox f 0 in
  let () = Pp.option ~with_space:false
      (fun f -> Format.fprintf f "%%def: \"seed\" \"%i\"@,") f conf.seed in
  let () = Format.fprintf
      f "%%def: \"syntaxVersion\" \"%i\"@,"
      (if conf.syntaxVersion = Ast.V4 then 4 else 3) in
  let () = Format.fprintf
      f "%%def: \"dumpIfDeadlocked\" \"%b\"@," conf.dumpIfDeadlocked in
  let () = Format.fprintf
      f "%%def: \"maxConsecutiveClash\" \"%i\"@," conf.maxConsecutiveClash in
  let () = Pp.option ~with_space:false
      (fun f -> Format.fprintf f "%%def: \"T0\" \"%g\"@,")
      f conf.initial in
  let () = Pp.option ~with_space:false
      (fun f -> function
         | Counter.DE i ->
           Format.fprintf f "%%def: \"plotPeriod\" \"%i\" \"events\"@," i
         | Counter.DT t ->
           Format.fprintf f "%%def: \"plotPeriod\" \"%g\" \"t.u.\"@," t)
      f conf.plotPeriod in
  let () = Pp.option ~with_space:false
      (fun f -> Format.fprintf f "%%def: \"outputFileName\" \"%s\"@,")
      f conf.outputFileName in
  let () = Pp.option ~with_space:false
      (fun f -> Format.fprintf f "%%def: \"traceFileName\" \"%s\"@,")
      f conf.traceFileName in
  let () = Pp.option ~with_space:false
      (fun f -> Format.fprintf f "%%def: \"deltaActivitiesFileName\" \"%s\"@,")
      f conf.deltaActivitiesFileName in
  Format.pp_close_box f ()
