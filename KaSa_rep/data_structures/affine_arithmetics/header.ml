(* header pour le Pi_Calcul   *)
open Hashtbl;;

let hashnumber=29000;;


(*type terminal = LCRO | RCRO | message | attente | replication
               | macro | et | ou | creation | FIN | ident of (string)
               | SEMICOLON | LPAREN | RPAREN  | VIRGULE | etiquette | equal | disequal;;*)

type name  = string ;;

type entry = Mute of string | Agent of (string*name*(name list)*
(name*int)) | Crea of string ;;

type id = string*int ;;

type in_out = Emission | Reception | Ressource 

type agent = {canal : id ; arguments : id list ; in_out : in_out ; etiquette : id}

type restriction = {channel:id} ;;

type switch = {equal : bool ; droit : id ; gauche : id ;label : id} ;;

type action = AGENT of agent | RESTRICTION of restriction | SWITCH
of switch ;;

type non_stan_agent = {can : int ; arg : int list ;
                       act : in_out };;

type non_stan_switch = {right : int;equ : bool;left : int};;



type non_stan_action = AG of (non_stan_agent) | SW of
(non_stan_switch) | ZERO ;;



type process = {new_names:int list;
		free_names:int list;
                communicated_names:int list; 
                synchronised_names:int list;
                todo : non_stan_action;
		fils : int list;
		cont : int list list};;

type redex = Ins of int | Out of int | Always of int | Egal of (int*int) | Inegal of (int*int) ;;

type terme = DISJUNCTION of (terme list)
          | CONJUNCTION of (terme list)
          | MACRO of (id*id list*terme)
          | ACTION of (action*terme)
          | EMPTY ;;

type lien = LIEN  of (id*(id list)*terme) ;;

type transition = Com of (int list*(int*int)) | Res of (int list);;

type channel= GLOBAL of string  | RESTRIC of (string*(int list)) | ARGS of (string*int list*int) ;;


type non_standard = {
 arbre:terme;
 globaux:(string*int) list;
 n_canaux:int;
 process_initiaux:int list;
 choix_initiaux:int list list;
 n_process:int;
 agents:process array};;








