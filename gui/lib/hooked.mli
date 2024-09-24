module DebugPrint : Kappa_webapp_lib_no_jsoo.Hooked.DebugPrint
module S : Kappa_webapp_lib_no_jsoo.Hooked.S
module E : Kappa_webapp_lib_no_jsoo.Hooked.E

type 'a signal = 'a S.t
type 'a event = 'a E.t
