type rule_id = int
type agent_id = int
type agent_type = int
type site = int
type state = int

type event =
| Dummy (* to avoid compilation warning *)
| Check_rule of rule_id
| See_a_new_bond of ((agent_type * site * state) * (agent_type * site * state))

type step =
  {
    site_out: site;
    site_in: site;
    agent_type_in: agent_type
  }

type path =
  {
    agent_id: agent_id;
    relative_address: step list;
    site: site;
  }

module type PathMap =
sig
  type 'a t
  val empty: 'a -> 'a t
  val add: path -> 'a -> 'a t -> 'a t
  val find: path -> 'a t -> 'a option
end

module PathMap:PathMap

type precondition

val dummy_precondition: precondition

val get_state_of_site:
  Exception.method_handler ->
  precondition ->
  path ->
  Exception.method_handler * precondition * int list Usual_domains.flat_lattice

(*fill in is_enable where it output the precondition, take the
  precondition, refine, the previous result, and output the new
  precondition*)
val refine_information_about_state_of_site:
  precondition ->
  (Exception.method_handler ->
   path ->
   int list Usual_domains.flat_lattice ->
   Exception.method_handler * int list Usual_domains.flat_lattice) ->
  precondition

val the_rule_is_applied_for_the_first_time:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  precondition ->
  Exception.method_handler * precondition

val the_rule_is_not_applied_for_the_first_time:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  precondition ->
  Exception.method_handler * precondition

val is_the_rule_applied_for_the_first_time:
  precondition -> Usual_domains.maybe_bool

type 'a fold =
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  agent_type ->
  site ->
  ((Remanent_parameters_sig.parameters ->
    state ->
    agent_type * site * state ->
    Exception.method_handler * 'a ->
    Exception.method_handler * 'a) ->
   'a ->
   Exception.method_handler * 'a) Usual_domains.flat_lattice

type prefold = { fold: 'a. 'a fold}

val overwrite_potential_partners_map:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  precondition ->
  (agent_type ->
   site ->
   state ->
   (agent_type * site * state) Usual_domains.flat_lattice)
  -> prefold ->
  Exception.method_handler * precondition

val get_potential_partner:
  precondition ->
  (agent_type -> site -> state -> precondition * (((agent_type * site * state) Usual_domains.flat_lattice)))



val fold_over_potential_partners:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  precondition ->
  agent_type ->
  site ->
  (Remanent_parameters_sig.parameters ->
   state ->
   agent_type * site * state ->
   Exception.method_handler * 'a -> Exception.method_handler * 'a) ->
  'a ->
  Exception.method_handler * precondition * 'a Usual_domains.top_or_not



