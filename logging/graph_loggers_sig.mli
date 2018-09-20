(**
  * graph_loggers_sig.ml
  *
  * a module for KaSim
  * Jérôme Feret, projet Antique, INRIA Paris
  *
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * Creation: 23/05/2016
  * Last modification: 25/05/2016
  * *
  *
  *
  * Copyright 2016  Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

type direction = Direct | Reverse | Undirected | Both
type shape = Invisible | House | Rect | Ellipse | Circle
type headkind = Normal | Vee | Tee | No_head
type linestyle = Plain | Dotted | Dashed

type color = Red | Green | White | Blue | Black | LightSkyBlue | PaleGreen | Brown

type options =
    | Color of color
    | FillColor of color
    | Label of string
    | Width of int (*pixel*)
    | Height of int (*pixel*)
    | Direction of direction
    | Shape of shape
    | ArrowHead of headkind
    | ArrowTail of headkind
    | LineStyle of linestyle
    | OnClick of Yojson.Basic.json
    | Contextual_help of string
    | Position of Locality.t list 

type graph =
    (string * options list) list
    *  (string * string * options list) list
