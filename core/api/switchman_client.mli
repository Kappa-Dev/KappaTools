type new_client
type mailbox

val new_mailbox: unit -> mailbox
val receive: mailbox -> string -> unit
val is_computing: mailbox -> bool 
