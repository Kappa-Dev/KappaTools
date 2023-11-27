(**
 * headers.ml
 * openkappa
 * Jérôme Feret, projet Abstraction/Antique, INRIA Paris-Rocquencourt
 *
 * Creation: 08/03/2010
 * Last modification: Time-stamp: <Nov 14 2016>
 * *
 * Some parameters
 * references can be tuned thanks to command-line options
 * other variables has to be set before compilation
 *
 * Copyright 2010,2011,2012,2013,2014,2015
 * Institut National de Recherche en Informatique et
 * en Automatique.  All rights reserved.  This file is distributed
 * under the terms of the GNU Library General Public License *)

(** if unsafe = true, then whenever an exception is raised, a default value is output, and no exception is raised*)
let dot_comment = "#"

let head parameters =
  [
    "This file has been computed by KaSa: a Static Analyzer for Kappa ("
    ^ Remanent_parameters.get_short_version parameters
    ^ ")";
    "Download sources/binaries at https://github.com/Kappa-Dev/KaSim";
    "";
    Remanent_parameters.get_launched_when_and_where parameters;
    "Command line is: "
    ^ String.concat " "
        (match
           Array.to_list (Remanent_parameters.get_command_line parameters)
         with
        | _t :: q -> "KaSa" :: q
        | [] -> []);
    "";
  ]

let dot_to_pdf =
  "Please use graphviz (http://www.graphviz.org) or OmniGraffle to export it \
   to a PDF"

let head_influence_map_in_dot =
  [
    "This file contains the description of the influence map in dot.";
    dot_to_pdf;
    "";
  ]

let head_contact_map_in_dot =
  [
    "This file contains the description of the contact map in dot.";
    dot_to_pdf;
    "";
  ]
