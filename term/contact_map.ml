type t = ((int list) * (int*int) list) array array

let print_kappa sigs f c =
  Format.fprintf f "@[<v>%a@]"
    (Pp.array Pp.space
       (fun ag f intf -> Format.fprintf f "@[<h>%%agent: %a(%a)@]"
           (Signature.print_agent sigs) ag
           (Pp.array Pp.comma
              (fun s f (is,ls) -> Format.fprintf f "%a%a%a"
                  (Signature.print_site sigs ag) s
                  (Pp.list Pp.empty
                     (fun f i -> Format.fprintf f "~%a"
                         (Signature.print_internal_state sigs ag s) i)) is
                  (Pp.list Pp.empty (fun f (ad,sd) -> Format.fprintf f "!%a.%a"
                                        (Signature.print_site sigs ad) sd
                                        (Signature.print_agent sigs) ad)) ls))
           intf))
    c
