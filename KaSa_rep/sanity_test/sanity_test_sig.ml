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

type ('data,'dicmvbdu,'diclist,'dicvlist) f = 
    Exception.method_handler -> 
    (bool Mvbdu_sig.cell -> bool Mvbdu_sig.cell -> int) -> 
    bool Mvbdu_sig.skeleton -> 
    bool Mvbdu_sig.cell ->  
    (int -> bool Mvbdu_sig.mvbdu) -> 
    ('data,'dicmvbdu,'diclist,'dicvlist,bool,int) Memo_sig.handler  ->  
    Exception.method_handler * 
      ((int * bool Mvbdu_sig.cell * bool Mvbdu_sig.mvbdu * 
          ('data,'dicmvbdu,'diclist,'dicvlist,bool,int) Memo_sig.handler) option)

type ('data,'dicmvbdu,'diclist,'dicvlist) g = 
    Exception.method_handler -> 
    (int List_sig.cell -> int List_sig.cell -> int) ->  
    int List_sig.skeleton -> 
    int List_sig.cell ->  
    (int -> int List_sig.list) -> 
    ('data,'dicmvbdu,'diclist,'dicvlist,bool,int) Memo_sig.handler  ->  
    Exception.method_handler * 
      ((int * int List_sig.cell * int List_sig.list * 
          ('data,'dicmvbdu,'diclist,'dicvlist,bool,int) Memo_sig.handler) option)

type ('data,'dicmvbdu,'diclist,'dicvlist) h = 
    Exception.method_handler -> 
    (unit List_sig.cell -> unit List_sig.cell -> int) ->  
    unit List_sig.skeleton -> 
    unit List_sig.cell ->  
    (int -> unit List_sig.list) -> 
    ('data,'dicmvbdu,'diclist,'dicvlist,bool,int) Memo_sig.handler  ->  
    Exception.method_handler * 
      ((int * unit List_sig.cell * unit List_sig.list * 
          ('data,'dicmvbdu,'diclist,'dicvlist,bool,int) Memo_sig.handler) option)
      
type ('mvbdu_handler,'dicmvbdu,'diclist,'dicvlist,'data) remanent = 
    {
      mvbdu_handler: 'mvbdu_handler;
      error: Exception.method_handler;
      output:out_channel;
      allocate_mvbdu: ('data,'dicmvbdu,'diclist,'dicvlist) f;
      allocate_uniquely_mvbdu: ('data,'dicmvbdu,'diclist,'dicvlist) f;
      allocate_association_list: ('data,'dicmvbdu,'diclist,'dicvlist) g;
      allocate_uniquely_association_list: ('data,'dicmvbdu,'diclist,'dicvlist) g;
      allocate_variables_list:  ('data,'dicmvbdu,'diclist,'dicvlist) h;
      allocate_uniquely_variables_list:  ('data,'dicmvbdu,'diclist,'dicvlist) h;
      parameters: Remanent_parameters_sig.parameters;
    }
      
let initial_remanent make_mvbdu_handler make_allocate_mvbdu make_allocate_association_list make_allocate_variables_list = 
  let error = Exception.empty_error_handler in  
  let error,handler = make_mvbdu_handler error in
  {
    output=stdout;
    mvbdu_handler=handler;
    error=error;
    parameters = Remanent_parameters.get_parameters ();
    allocate_mvbdu = make_allocate_mvbdu false;
    allocate_uniquely_mvbdu = make_allocate_mvbdu true;
    allocate_association_list = make_allocate_association_list false ;
    allocate_uniquely_association_list = make_allocate_association_list true ;
    allocate_variables_list = make_allocate_variables_list false ;
    allocate_uniquely_variables_list = make_allocate_variables_list true ;
    
  }
