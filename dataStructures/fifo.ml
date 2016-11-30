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
  * Institut National de Recherche en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type 'a t =
  {
    waiting_elts: 'a list ;
    list: 'a list ;
  }

let empty =
  {
    waiting_elts = [];
    list = []
  }

let is_empty t =
  t.waiting_elts = [] && t.list = [] 

let push a t = { t with waiting_elts = a::t.waiting_elts}

let rec pop t =
  match
    t.list
  with
  | head::tail ->
    {t with list = tail},Some head
  | [] ->
    begin
      match t.waiting_elts with
      | [] -> t, None
      | list -> pop {waiting_elts = [] ; list = List.rev list}
    end
