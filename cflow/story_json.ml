type new_story =
  {
    id: int;
    graph: Graph_loggers_sig.graph;
  }

type story =
  | New of new_story
  | Same_as of int

type 'a one_compression =
  {
    log_info: 'a Trace.Simulation_info.t list;
    story: story
  }

let string_of_json = function
  | `String s -> s
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct string",x))

let int_of_json = function
  | `Int s -> s
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct int",x))

let float_of_json = function
  | `Float s -> s
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct float",x))

let new_story_to_json new_story =
  `Assoc
    [
      "id", `Int new_story.id;
      "graph", Graph_json.to_json new_story.graph;
    ]

let new_story_of_json = function
  | `Assoc l as x when List.length l = 2 ->
    begin
      try
        {
          id = int_of_json (List.assoc "id" l);
          graph = Graph_json.of_json (List.assoc "graph" l)
        }
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Not a correct new story",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct new story",x))

let story_to_json story =
  match
    story
  with
  | New new_story ->
    `Assoc
      [
        "new", new_story_to_json new_story
      ]
  | Same_as int ->
    `Assoc
      [
        "same_as", `Int int
      ]


let story_of_json = function
  | `Assoc ["new", new_story] -> New (new_story_of_json new_story)
  | `Assoc ["same_as", `Int int] -> Same_as int
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct story",x))

let to_json log_info_to_json one_compression =
  `Assoc
    [
      "log_info",
      `List
        (List.rev_map
           (Trace.Simulation_info.to_json log_info_to_json)
           (List.rev one_compression.log_info));
      "story",
      story_to_json one_compression.story
    ]

let of_json log_info_of_json = function
  | `Assoc l as x when List.length l = 2 ->
    begin
      try
        {
          log_info =
            begin
              match List.assoc "log_info" l
              with
              | `List l ->
                List.rev_map
                  (Trace.Simulation_info.of_json
                     log_info_of_json)
                  (List.rev l)
              | _y ->
                raise (Yojson.Basic.Util.Type_error ("Not a correct story computation",x))
            end
          ;
          story = story_of_json
              (List.assoc "story" l)
        }
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Not a correct story computation",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct story computation",x))

type phase =
  | Start
  | Inprogress
  | Success
  | Faillure

type status =
  {
    phase: phase;
    message: string;
  }

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
  | x ->  raise (Yojson.Basic.Util.Type_error ("Not a correct phase",x))

let status_to_json status =
  `Assoc
    [
      "phase", phase_to_json status.phase;
      "message", `String status.message ;
    ]


let status_of_json = function
  | `Assoc l as x when List.length l = 2 ->
    begin
      try
        {
          phase = phase_of_json (List.assoc "phase" l);
          message = string_of_json (List.assoc "message" l);
        }
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Not a correct status",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct status",x))

type progress_bar =
  {
    bool: string;
    current: int;
    total: int
  }

let progress_bar_to_json progress_bar =
  `Assoc ["progress_bar",
          `Assoc ["bool",`String progress_bar.bool;
                  "current",`Int progress_bar.current;
                  "total",`Int progress_bar.total]
         ]

let progress_bar_of_json = function
  | `Assoc l as x when List.length l = 1 ->
    begin
      match List.assoc "progress_bar" l with
      | `Assoc l when List.length l = 3 ->
        begin
          try
            {
              bool = string_of_json (List.assoc "bool" l);
              current = int_of_json (List.assoc "current" l);
              total = int_of_json (List.assoc "total" l)
            }
          with Not_found ->
            raise (Yojson.Basic.Util.Type_error ("Not a correct progress bar",x))
        end
      | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct progress bar",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct progress bar",x))

type 'a message =
  | Phase of phase * string
  | Progress of progress_bar
  | Story of 'a one_compression

let message_to_json = function
  | Phase (p,m) -> `List [ `String "PHASE"; phase_to_json p; `String m ]
  | Progress p -> `List [ `String "PROGRESS"; progress_bar_to_json p ]
  | Story s -> `List [ `String "STORY"; to_json (fun _ -> `Null) s ]

let message_of_json = function
  | `List [ `String "PHASE"; p; `String m ] -> Phase (phase_of_json p, m)
  | `List [ `String "PROGRESS"; p ] -> Progress (progress_bar_of_json p)
  | `List [ `String "STORY"; s ] -> Story (of_json (fun _ -> ()) s)
  | x -> raise (Yojson.Basic.Util.Type_error ("Invalid story message",x))
