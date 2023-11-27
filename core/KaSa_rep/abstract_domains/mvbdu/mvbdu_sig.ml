(**
    * mvbdu_sig.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    *
    * Creation: 08/11/2010
    * Last modification: Time-stamp: <Jul 02 2016>
    * *
    * Signature for  primitives to deal set of finite maps from integers to integers
    *
    * Copyright 2010 Institut National de Recherche en Informatique et
    * en Automatique.  All rights reserved.  This file is distributed
    * under the terms of the GNU Library General Public License *)

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
