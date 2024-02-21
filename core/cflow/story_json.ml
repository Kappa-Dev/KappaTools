type current_compression_mode = Weak | Strong | Causal
type new_story = { id: int; graph: Graph_loggers_sig.graph }
type story = New of new_story | Same_as of int

type 'a one_compression = {
  log_info: 'a Trace.Simulation_info.t list;
  story_mode: current_compression_mode;
  story: story;
}

let string_of_json = function
  | `String s -> s
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct string", x))

let int_of_json = function
  | `Int s -> s
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct int", x))

let new_story_to_json new_story =
  `Assoc
    [ "id", `Int new_story.id; "graph", Graph_json.to_json new_story.graph ]

let new_story_of_json = function
  | `Assoc l as x when List.length l = 2 ->
    (try
       {
         id = int_of_json (List.assoc "id" l);
         graph = Graph_json.of_json (List.assoc "graph" l);
       }
     with Not_found ->
       raise (Yojson.Basic.Util.Type_error ("Not a correct new story", x)))
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct new story", x))

let story_to_json story =
  match story with
  | New new_story -> `Assoc [ "new", new_story_to_json new_story ]
  | Same_as int -> `Assoc [ "same_as", `Int int ]

let story_of_json = function
  | `Assoc [ ("new", new_story) ] -> New (new_story_of_json new_story)
  | `Assoc [ ("same_as", `Int int) ] -> Same_as int
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct story", x))

let story_mode_to_json = function
  | Causal -> `String "Causal"
  | Weak -> `String "Weak"
  | Strong -> `String "Strong"

let story_mode_of_json = function
  | `String "Causal" -> Causal
  | `String "Weak" -> Weak
  | `String "Strong" -> Strong
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct story mode", x))

let to_json log_info_to_json one_compression =
  `Assoc
    [
      ( "log_info",
        `List
          (List.rev_map
             (Trace.Simulation_info.to_json log_info_to_json)
             (List.rev one_compression.log_info)) );
      "story_mode", story_mode_to_json one_compression.story_mode;
      "story", story_to_json one_compression.story;
    ]

let of_json log_info_of_json = function
  | `Assoc l as x when List.length l = 3 ->
    (try
       {
         log_info =
           (match List.assoc "log_info" l with
           | `List l ->
             List.rev_map
               (Trace.Simulation_info.of_json log_info_of_json)
               (List.rev l)
           | _y ->
             raise
               (Yojson.Basic.Util.Type_error
                  ("Not a correct story computation", x)));
         story = story_of_json (List.assoc "story" l);
         story_mode = story_mode_of_json (List.assoc "story_mode" l);
       }
     with Not_found ->
       raise
         (Yojson.Basic.Util.Type_error ("Not a correct story computation", x)))
  | x ->
    raise (Yojson.Basic.Util.Type_error ("Not a correct story computation", x))

type phase = Start | Inprogress | Success | Faillure

let start = "starting computation"
let inprogress = "computation in progress"
let success = "computation completed successfully"
let faillure = "computation (partially) failed"

let phase_to_json = function
  | Start -> `String start
  | Inprogress -> `String inprogress
  | Success -> `String success
  | Faillure -> `String faillure

let phase_of_json = function
  | `String s when s = start -> Start
  | `String s when s = inprogress -> Inprogress
  | `String s when s = success -> Success
  | `String s when s = faillure -> Faillure
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct phase", x))

type progress_bar = { bool: string; current: int; total: int }

let progress_bar_to_json progress_bar =
  `Assoc
    [
      ( "progress_bar",
        `Assoc
          [
            "bool", `String progress_bar.bool;
            "current", `Int progress_bar.current;
            "total", `Int progress_bar.total;
          ] );
    ]

let progress_bar_of_json = function
  | `Assoc l as x when List.length l = 1 ->
    (match List.assoc "progress_bar" l with
    | `Assoc l when List.length l = 3 ->
      (try
         {
           bool = string_of_json (List.assoc "bool" l);
           current = int_of_json (List.assoc "current" l);
           total = int_of_json (List.assoc "total" l);
         }
       with Not_found ->
         raise (Yojson.Basic.Util.Type_error ("Not a correct progress bar", x)))
    | x ->
      raise (Yojson.Basic.Util.Type_error ("Not a correct progress bar", x)))
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct progress bar", x))

type 'a message =
  | Phase of phase * string
  | Progress of progress_bar
  | Story of 'a one_compression

let message_to_json = function
  | Phase (p, m) -> `List [ `String "PHASE"; phase_to_json p; `String m ]
  | Progress p -> `List [ `String "PROGRESS"; progress_bar_to_json p ]
  | Story s -> `List [ `String "STORY"; to_json (fun _ -> `Null) s ]

let message_of_json = function
  | `List [ `String "PHASE"; p; `String m ] -> Phase (phase_of_json p, m)
  | `List [ `String "PROGRESS"; p ] -> Progress (progress_bar_of_json p)
  | `List [ `String "STORY"; s ] -> Story (of_json (fun _ -> ()) s)
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid story message", x))
