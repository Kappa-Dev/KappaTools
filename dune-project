(lang dune 2.9)
(generate_opam_files)

(name kappa-tool-suite)
(version 4.1.3) ; could be removed and generated at release time instead using dune-release
(source (github Kappa-Dev/KappaTools)) ; no need to specifybug-reports and dev-repo
(license LGPL-3.0-only)
(authors
  "Jean Krivine" "Jérôme Feret" "Pierre Boutillier"
  "Ioana Cristescu" "Mutaamba Maasha" "Lý Kim Quyên" "Antoine Pouille")
(maintainers "Antoine Pouille <antoine.a.pouille@inria.fr>"
             "Jérôme Feret <jerome.feret@info.ens.psl.eu>")
(homepage https://kappalanguage.org/)
(documentation https://kappalanguage.org/sites/kappalanguage.org/files/inline-files/Kappa_Manual.pdf)

(package
  (name kappa-library)
  (synopsis "Public internals of the Kappa tool suite. Use this package to use kappa as a lib")
  (depends
   (ocaml ( >= 4.05.0 ))
   (yojson ( >= 2.0 ))
   (lwt ( >= 4.2.0 ))
   num
   re
   fmt
   logs
   stdlib-shims
   result
   ; If adding unpinned libs: consider pinning them in binaries below
   )
)

; pinned versions, TODO: remove when added to new opam packages
;  (ocaml ( = 4.14.2 ))
;  (atdgen ( = 2.15.0 ))
;  (atdgen-runtime ( = 2.15.0 ))
;  (dune ( = 3.14.0 ))
;  (fmt ( = 0.9.0 ))
;  (js_of_ocaml ( = 5.7.0 ))
;  (js_of_ocaml-lwt ( = 5.7.0 ))
;  (js_of_ocaml-ppx ( = 5.7.0 ))
;  (js_of_ocaml-tyxml ( = 5.7.0 ))
;  (logs ( = 0.7.0 ))
;  (lwt ( = 5.7.0 ))
;  (lwt_react ( = 1.2.0 ))
;  (num ( = 1.5 ))
;  (re ( = 1.11.0 ))
;  (result ( = 1.5 ))
;  (stdlib-shims ( = 0.3.0 ))
;  (tyxml-ppx ( = 4.6.0 ))
;  (yojson ( = 2.1.0 ))

(package
  (name kappa-binaries)
  (synopsis "Command line interfaces of the Kappa tool suite. Use this package if you want the CLI kappa tools")
  (depends
   (ocaml ( = 4.14.2 ))
   (yojson ( = 2.1.0 ))
   (lwt ( = 5.7.0 ))
   (num ( = 1.5 ))
   (re ( = 1.11.0 ))
   (fmt ( = 0.9.0 ))
   (logs ( = 0.7.0 ))
   kappa-library
   )
  (depopts
    labltk)
)

(package
  (name kappa-agents)
  (synopsis "Backends for an interactive use of the Kappa tool suite. Used by the kappa-webapp, probably not relevant else. Previously used by deprecated kappa-server")
  (depends
   (ocaml ( = 4.14.2 ))
   (yojson ( = 2.1.0 ))
   (lwt ( = 5.7.0 ))
   (num ( = 1.5 ))
   (re ( = 1.11.0 ))
   (fmt ( = 0.9.0 ))
   (logs ( = 0.7.0 ))
   (atdgen ( = 2.15.0 )) ; TODO: find a way to specify as build dependency only, as with the `build` package variable in opam
   (atdgen-runtime ( = 2.15.0 ))
   kappa-library
   )
)

(package
  (name kappa-webapp)
  (synopsis "Browser app for an interactive use of the Kappa tool suite. Best way to start with the kappa language")
  (depends
   (ocaml ( >= 4.05.0 ))
   (yojson ( >= 1.6.0 ))
   (lwt ( >= 4.2.0 ))
   num
   re
   fmt
   logs
   atdgen-runtime
   atdgen ; TODO: find a way to specify as build dependency only, as with the `build` package variable in opam
   lwt_react
   tyxml-ppx
   js_of_ocaml
   js_of_ocaml-ppx
   js_of_ocaml-lwt
   js_of_ocaml-tyxml
   kappa-binaries
   kappa-agents
   )
  ; does not build executables as there seem to be no way to build js exes in a package
)
