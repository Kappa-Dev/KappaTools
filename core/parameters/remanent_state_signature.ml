(**
    * remanent_state_signature.ml
    *
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    *
    * Creation: 12/10/2010
    * Last modification: Time-stamp: <Nov 28 2018>
    * *
    * Signature for the current state of the memory
    *
    * Copyright 2010 Institut National de Recherche en Informatique et
    * en Automatique.  All rights reserved.  This file is distributed
    * under the terms of the GNU Library General Public License *)

type rule_key = int

type engine_state = {
  command_line: string option;
  wake_up_map: (rule_key -> rule_key list) option;
}

let empty_engine_state = { command_line = None; wake_up_map = None }
let wake_up_map error engine_state = error, engine_state.wake_up_map
