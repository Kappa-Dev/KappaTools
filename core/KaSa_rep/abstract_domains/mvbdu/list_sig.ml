(**
    * mvbdu_sig.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    *
    * Creation: 22/11/2010
    * Last modification: Time-stamp: <Jul 02 2016>
    * *
    * Signature for  primitives to deal with list of pairs (variable,'a)
    *
    * Copyright 2010 Institut National de Recherche en Informatique et
    * en Automatique.  All rights reserved.  This file is distributed
    * under the terms of the GNU Library General Public License *)

type variable = Mvbdu_sig.variable
type hash_key = Mvbdu_sig.hash_key

type ('a, 'b) precell = { variable: variable; association: 'b; tail: 'a }
and 'a prelist = Empty | Cons of 'a
and 'a cell = ('a list, 'a) precell prelist
and 'a list = { id: hash_key; value: 'a cell }
and 'a skeleton = (hash_key, 'a) precell prelist
