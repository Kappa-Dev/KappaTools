type variable = Mvbdu_sig.variable
type hash_key = Mvbdu_sig.hash_key

type ('a, 'b) precell = { variable: variable; association: 'b; tail: 'a }
and 'a prelist = Empty | Cons of 'a
and 'a cell = ('a list, 'a) precell prelist
and 'a list = { id: hash_key; value: 'a cell }
and 'a skeleton = (hash_key, 'a) precell prelist
