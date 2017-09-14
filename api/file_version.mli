(* return an empty file version *)
val empty : Api_types_j.file_version
(* create initalized version for a client *)
val create : Api_types_j.client_id -> Api_types_j.file_version
(* increment a version for a client *)
val increment :
  Api_types_j.client_id -> Api_types_j.file_version -> Api_types_j.file_version
(* merge two file versions *)
val merge :
  Api_types_j.file_version -> Api_types_j.file_version -> Api_types_j.file_version
(* determine if one vector is greater than another *)
val gt :
  ?client_id:Api_types_j.client_id ->
  Api_types_j.file_version -> Api_types_j.file_version -> bool
