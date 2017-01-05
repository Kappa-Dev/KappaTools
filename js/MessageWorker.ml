(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type id = int

type request = Create
             | Parse of id * Api_types.code
             | Start of id * Api_types.parameter
             | Status of id * Api_types.token
             | List of id
             | Stop of id *Api_types.token
type response = Create of id * Api_types.error
              | Parse of id * Api_types.token result
              | Status of id * Api_types.state result
              | List of id * Api_types.catalog result
              | Stop of unit result
