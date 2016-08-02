type new_story =
  {
    id: int;
    graph: Graph_loggers_sig.graph;
  }

type story =
  | New of new_story
  | Same_as of int

type one_compression =
  {
    log_info: StoryProfiling.StoryStats.log_info Trace.Simulation_info.t list;
    story: story
  }

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
          id =
            begin
              match List.assoc "id" l
              with
                `Int int -> int
              | _y ->
                raise (Yojson.Basic.Util.Type_error ("Not a correct new story",x))
            end;
          graph =
            Graph_json.of_json (List.assoc "graph" l)
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

let to_json one_compression =
  `Assoc
    [
      "log_info",
      `List
        (List.rev_map
           (Trace.Simulation_info.to_json StoryProfiling.StoryStats.log_info_to_json)
           (List.rev one_compression.log_info));
      "story",
      story_to_json one_compression.story
    ]

let of_json = function
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
                     StoryProfiling.StoryStats.log_info_of_json)
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
          message =
            begin
              match List.assoc "message" l
              with
              | `String s -> s
              | _y ->
                raise (Yojson.Basic.Util.Type_error ("Not a correct status",x))
            end
          ;
        }
      with Not_found ->
        raise (Yojson.Basic.Util.Type_error ("Not a correct status",x))
    end
  | x -> raise (Yojson.Basic.Util.Type_error ("Not a correct status",x))
