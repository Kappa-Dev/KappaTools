 (**
    * sanity_test.ml
    * openkappa
    * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
    * 
    * Creation: 2010, the 28th of March
    * Last modification: 2011, the 23rd of March
    * * 
    * This library provides a bench of run time tests.
    *  
    * Copyright 2010 Institut National de Recherche en Informatique et   
    * en Automatique.  All rights reserved.  This file is distributed     
    * under the terms of the GNU Library General Public License *)

type ('data,'dicmvbdu,'diclist) f = Exception.method_handler -> 
                (bool Mvbdu_sig.cell -> bool Mvbdu_sig.cell -> int) -> 
                bool Mvbdu_sig.skeleton -> 
                bool Mvbdu_sig.cell ->  
                (int -> bool Mvbdu_sig.mvbdu) -> 
                ('data,'dicmvbdu,'diclist,bool,int) Memo_sig.handler  ->  
                Exception.method_handler * ((int * bool Mvbdu_sig.cell * bool Mvbdu_sig.mvbdu * ('data,'dicmvbdu,'diclist,bool,int) Memo_sig.handler) option)

type ('data,'dicmvbdu,'diclist) g = Exception.method_handler -> 
                (int List_sig.cell -> int List_sig.cell -> int) ->  
                int List_sig.skeleton -> 
                int List_sig.cell ->  
                (int -> int List_sig.list) -> 
                ('data,'dicmvbdu,'diclist,bool,int) Memo_sig.handler  ->  
                Exception.method_handler * ((int * int List_sig.cell * int List_sig.list * ('data,'dicmvbdu,'diclist,bool,int) Memo_sig.handler) option)
  
type ('mvbdu_handler,'dicmvbdu,'diclist,'data) remanent = 
  {
    mvbdu_handler: 'mvbdu_handler;
    error: Exception.method_handler;
    output:out_channel;
    allocate_mvbdu: ('data,'dicmvbdu,'diclist) f;
    allocate_uniquely_mvbdu: ('data,'dicmvbdu,'diclist) f;
    allocate_list: ('data,'dicmvbdu,'diclist) g;
    allocate_uniquely_list: ('data,'dicmvbdu,'diclist) g;
    parameters: Remanent_parameters_sig.parameters;
          }
  
let initial_remanent make_mvbdu_handler make_allocate_mvbdu make_allocate_list = 
  let error = Exception.empty_error_handler in  
  let error,handler = make_mvbdu_handler error in
    {
      output=stdout;
      mvbdu_handler=handler;
      error=error;
      parameters = Remanent_parameters.get_parameters ();
      allocate_mvbdu = make_allocate_mvbdu false;
      allocate_uniquely_mvbdu = make_allocate_mvbdu true;
      allocate_list = make_allocate_list false ;
      allocate_uniquely_list = make_allocate_list true
        
      }

