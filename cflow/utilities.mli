(**
  * utilities.mli  
  *
  * Causal flow compression: a module for KaSim 
  * Jerome Feret, projet Abstraction, INRIA Paris-Rocquencourt
  * Jean Krivine, Université Paris-Diderot, CNRS 
  * 
  * KaSim
  * Jean Krivine, Universite Paris-Diderot, CNRS 
  *  
  * Creation: 10/08/2015
  * Last modification: 10/08/2015
  * * 
  * Some functionalities for story compression
  *  
  * Copyright 2011,2012,2013 Institut National de Recherche en Informatique 
  * et en Automatique.  All rights reserved.  This file is distributed     
  * under the terms of the GNU Library General Public License *)

module D:Dag.Dag

type ('a,'b,'c) remanent =  
  D.S.PH.B.PB.CI.Po.K.H.error_channel * int * (bool * int * int) *
    D.S.PH.B.blackboard *
      (D.prehash *
         (Causal.grid * D.graph * 'a option *
            ('b * D.S.PH.update_order list *
               D.S.PH.B.PB.CI.Po.K.refined_step list) *
              D.S.PH.B.PB.CI.Po.K.step list *
		'c Mods.simulation_info option list)
           list)
        list * int
	   
val from_none_to_weak_with_tick:
  D.S.PH.B.PB.CI.Po.K.H.parameter ->
  D.S.PH.B.PB.CI.Po.K.H.handler ->
  D.S.PH.B.PB.CI.Po.K.P.log_info ->
  Format.formatter ->
  int ->
  ('a,'b,'c) remanent ->
  ('b * 'd * D.S.PH.B.PB.CI.Po.K.refined_step list) * 'e * 'c Mods.simulation_info option list ->
  ('a,'b,'c) remanent
    
val from_none_to_weak_with_tick_ext:
       D.S.PH.B.PB.CI.Po.K.H.parameter ->
           D.S.PH.B.PB.CI.Po.K.H.handler ->
           D.S.PH.B.PB.CI.Po.K.P.log_info ->
           Format.formatter ->
           int ->
           ('a,'b,'c) remanent ->
	   'd * 'e * 'f * ('b * 'g * D.S.PH.B.PB.CI.Po.K.refined_step list) *
           'h * 'c Mods.simulation_info option list ->
           ('a,'b,'c) remanent
	     
		      
