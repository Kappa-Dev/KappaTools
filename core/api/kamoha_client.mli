type mailbox

val new_mailbox : unit -> mailbox
val receive : mailbox -> string -> unit
val is_computing : mailbox -> bool

class virtual new_client : post:(string -> unit) -> mailbox -> Api.manager_model
