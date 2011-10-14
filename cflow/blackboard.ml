(**
  * blackboard.ml
  *
  * Causal flow compression: a module for KaSim 
  * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Université Paris Dederot, CNRS 
  *  
  * Creation: 06/09/2011
  * Last modification: 06/09/2011
  * * 
  * Some parameters references can be tuned thanks to command-line options
  * other variables has to be set before compilation   
  *  
  * Copyright 2011 Institut National de Recherche en Informatique et   
  * en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

type kasim_agent
type kasim_site 
type kasim_event 

type column_id = int 
module ColumnIdSet = Set.Make (struct type t = column_id let compare = compare end)
type event_id = int 
module EidMap = Map.Make (struct type t = event_id let compare = compare end)
type event_short_id = int 

type event = Kasim of kasim_event | Pseudo_event
type column = Here | Bound_type of agent * site | Bound_or_not of agent * site | Bound_to of agent * site 

module ColumnMap = Map.Make (struct type t = column let compare = compare end)

type case_address = 
    {
      column_id:column_id; 
      row_id:   event_id;
    }

type case_value = Undef | Free | State of int 

type case_info = 
    {
      row_short_id: event_short_id;
      state_before: case_value option;
      state_after: case_value option
    }


type preblackboard = 
    {
      sparse_matrix: case array array ;
      nevents_by_column: event_id array;
      nevents: event_id;
      ncolumn: column_id;
      column_map: column_id ColumnMap.t;
      columns_of_eid: column_id list array; 
      row_short_id_map : event_short_id EidMap.t array;
      previous_binding: ColumnIdSet.t array;
      configuration: ColumnIdMap.t
    }


      


