type store = {
  file: string;
  title: string;
  descr: string;
  legend: string array;
  mutable points: (float * Nbr.t array) list;
}

val to_string : ?width:int -> store -> string
val to_file : store -> unit
