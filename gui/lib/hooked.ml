module DebugPrint : Kappa_webapp_lib_no_jsoo.Hooked.DebugPrint = struct
  let debug_print s =
    let () = Common.debug ~loc:__LOC__ s in
    ()
end

module S = Kappa_webapp_lib_no_jsoo.Hooked.MakeS (DebugPrint)
module E = Kappa_webapp_lib_no_jsoo.Hooked.MakeE (DebugPrint)

type 'a signal = 'a S.t
type 'a event = 'a E.t
