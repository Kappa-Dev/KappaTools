(**
  * fifo.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, Antique project, INRIA Paris
  *
  * Creation: 2016, the 30th of November
  * Last modification: Time-stamp: <Nov 30 2016>
  *
  * FIFO
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016
  * Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)



type 'a t

val empty:  'a t
val is_empty : 'a t -> bool
val push :  'a  -> 'a t ->  'a t
val pop : 'a t -> 'a t * 'a option
