type ('a, 'b) t = {
  int_of_obs: 'b -> int;
  int_of_kappa_instance: 'a -> int;
  int_of_token_id: 'b -> int;
}
