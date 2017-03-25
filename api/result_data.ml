(* utility for results *)
type ('ok,'error) t = ('ok,'error) Api_types_j.result_data
let map
    ~(ok:'ok -> 'a)
    ~(error:'error -> 'a) : ('ok,'error) t -> 'a
  =
  function
  | `Ok o -> ok o
  | `Error e -> error e

let bind
    ~(ok:'ok -> 'a) : ('ok,'error) t -> 'a
  =
  function
  | `Ok o -> ok o
  | `Error e -> `Error e

let error (error:'error ) : ('ok,'error) t = `Error error

let ok (ok : 'ok) : ('ok,'error) t = `Ok ok
