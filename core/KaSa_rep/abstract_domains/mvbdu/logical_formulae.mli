type 'a formula =
  | P of 'a
  | NOT of 'a formula
  | IMPLY of 'a formula * 'a formula
  | AND of 'a formula * 'a formula
  | OR of 'a formula * 'a formula
  | False
  | True

val print :
  Remanent_parameters_sig.parameters ->
  'error ->
  ('error -> 'a -> 'error * string) ->
  'a formula ->
  'error

val simplify : 'a formula -> 'a formula
