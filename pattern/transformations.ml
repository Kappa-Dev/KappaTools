type place = Connected_component.node Connected_component.place
type t =
    Freed of place * int
  | Linked of (place * int) * (place * int)
  | Internalized of place * int * int

let rename wk cc inj = function
  | Freed (p,s) as x ->
     let p' = Connected_component.rename_place wk cc inj p in
     if p == p' then x else Freed (p',s)
  | Linked ((p1,s1),(p2,s2)) as x ->
     let p1' = Connected_component.rename_place wk cc inj p1 in
     let p2' = Connected_component.rename_place wk cc inj p2 in
     if p1 == p1' && p2 == p2' then x else Linked ((p1',s1),(p2',s2))
  | Internalized (p,s,i) as x ->
     let p' = Connected_component.rename_place wk cc inj p in
     if p == p' then x else Internalized (p',s,i)

let print sigs f = function
  | Freed (p,s) ->
     Format.fprintf
       f "@[%a.%a = %t@]"
       (Connected_component.print_place sigs) p
       (Connected_component.print_place_site sigs p) s
		    Pp.bottom
  | Linked ((p1,s1),(p2,s2)) ->
     Format.fprintf
       f "@[%a.%a = %a.%a@]"
       (Connected_component.print_place sigs) p1
       (Connected_component.print_place_site sigs p1) s1
       (Connected_component.print_place sigs) p2
       (Connected_component.print_place_site sigs p2) s2
  | Internalized (p,s,i) ->
     Format.fprintf
       f "@[%a.%a =@]"
       (Connected_component.print_place sigs) p
       (Connected_component.print_place_internal sigs p s) i
