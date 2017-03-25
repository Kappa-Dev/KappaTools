type ('ok,'error) t
val map :
  ok:('ok -> 'a) ->
  error:('error -> 'a) -> ('ok, 'error) t -> 'a
val bind :
  ok:('ok -> ([> `Error of 'error ] as 'a)) ->
  ('ok, 'error) t -> 'a
val error : 'error -> ('ok, 'error) t
val ok : 'ok -> ('ok, 'error) t
