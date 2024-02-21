type variable = int (*Ckappa_sig.c_site_name*)
type upper_bound = int
type hash_key = int

type ('a, 'b) precell = {
  variable: variable;
  upper_bound: upper_bound;
  branch_true: 'a;
  branch_false: 'a;
}

and ('a, 'b) premvbdu = Leaf of 'b | Node of 'a
and 'b mvbdu = { id: hash_key; value: 'b cell }
and 'b skeleton = ((hash_key, 'b) precell, 'b) premvbdu
and 'b cell = (('b mvbdu, 'b) precell, 'b) premvbdu
