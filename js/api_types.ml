type error = string list
type token = int
type catalog = int list
type code = string
type parameter = { code : code;
                   nb_plot : int;
                   max_time : float option;
                   max_events : int option
                 }

type state = { plot : string ;
               time : float;
               time_percentage : int option;
               event : int;
               event_percentage : int option;
               tracked_events : int option;
               log_messages : string list;
               is_running : bool
             }
