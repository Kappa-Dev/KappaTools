(* utility for results *)
let result_map
    ~(ok:Api_types_j.result_code -> 'ok -> 'a)
    ~(error:Api_types_j.result_code -> Api_types_j.errors -> 'a)
    ~(result:'ok Api_types_j.result)
  : 'a =
  match result.Api_types_j.data with
  | `Ok data -> ok result.Api_types_j.result_code data
  | `Error data -> error result.Api_types_j.result_code data

let result_bind
    ~(ok:'ok -> 'a Api_types_j.result)
    ~(result:'ok Api_types_j.result)
  : 'a Api_types_j.result =
  match result.Api_types_j.data with
  | `Ok data -> ok data
  | `Error data ->
    { Api_types_j.data = `Error data ;
      Api_types_j.result_code = result.Api_types_j.result_code }

let result_ok ?(result_code:Api_types_j.result_code = 200)
    (ok:'ok) : 'ok Api_types_j.result =
  { Api_types_j.data = `Ok ok ;
    Api_types_j.result_code = result_code }

let result_error_msg
    ?(severity:Api_types_j.severity = `Error)
    ?(result_code:Api_types_j.result_code = 400)
    (message:string) : 'ok Api_types_j.result =
  { Api_types_j.data = `Error [{ Api_types_j.severity = severity;
				 Api_types_j.message = message;
				 Api_types_j.range = None }];
    Api_types_j.result_code = result_code }
