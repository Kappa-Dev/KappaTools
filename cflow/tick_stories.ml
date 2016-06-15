(**
   * tick_stories.ml
   *
   * Progress bar (coming from Mods.ml)
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   * Jean Krivine, Université Paris-Diderot, CNRS
   *
   * KaSim
   * Jean Krivine, Université Paris Dederot, CNRS
   *
   * Creation: 17/05/2016
   * Last modification: 17/05/2016
   * *
   *
   * Copyright 2011,2012,2013 Institut National de Recherche en Informatique
   * et en Automatique.  All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)


let tick_stories f save_progress_bar n_stories (init,last,counter) =
  let () =
    if not init then
      let c = ref !Parameter.progressBarSize in
      let () = Loggers.print_newline f in
      while !c > 0 do
        Loggers.fprintf  f "_" ;
        c:=!c-1
      done ;
      Loggers.print_newline f
  in
  let n =
    if n_stories <=0 && counter = 0
    then !Parameter.progressBarSize
    else if counter > n_stories
    then 0
    else
      let nc = (counter * !Parameter.progressBarSize) / n_stories in
      let nl = (last * !Parameter.progressBarSize) / n_stories in
      nc - nl
  in
  let rec aux n =
    if n<=0 then ()
    else
      let () = Loggers.fprintf f "%c" (!Parameter.progressBarSymbol) in
      let () = if !Parameter.eclipseMode then Loggers.print_newline f in
      aux (n-1)
  in
  let () = aux n in
  let () = Loggers.flush_logger f in
  let () = if counter = n_stories then Loggers.print_newline f in
  let bar = (true,counter,counter+1) in
  let () = save_progress_bar n_stories bar in
  bar
