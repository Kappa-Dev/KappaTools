type api_version = V2

type t = {
  mutable api: api_version;
  mutable log_channel: Lwt_io.output_channel option;
}

val default : t
val options : t -> (string * Arg.spec * string) list
