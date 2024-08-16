(lang dune 2.9)
(generate_opam_files)

(name kappa-tool-suite)
(version 4.1~dev) ; could be removed and generated at release time instead using dune-release
(source (github Kappa-Dev/KappaTools)) ; no need to specifybug-reports and dev-repo
(license LGPL-3.0-only)
(authors
  "Jean Krivine" "Jérôme Feret" "Pierre Boutillier"
  "Ioana Cristescu" "Mutaamba Maasha" "Lý Kim Quyên" "Antoine Pouille")
(maintainers "Antoine Pouille <antoine.a.pouille@inria.fr>"
             "Pierre Boutillier <pierre.boutillier@laposte.net>")
(homepage https://kappalanguage.org/)
(documentation https://kappalanguage.org/sites/kappalanguage.org/files/inline-files/Kappa_Manual.pdf)

(package
  (name kappa-library)
  (synopsis "Public internals of the Kappa tool suite")
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

(package
  (name kappa-binaries)
  (synopsis "Command line interfaces of the Kappa tool suite")
  (depends
   (ocaml ( >= 4.05.0 ))
   (yojson ( >= 1.6.0 ))
   (lwt ( >= 4.2.0 ))
   num
   re
   fmt
   logs
   kappa-library
   )
  (depopts
    labltk)
)

(package
  (name kappa-agents)
  (synopsis "Backends for an interactive use of the Kappa tool suite")
  (depends
   (ocaml ( >= 4.05.0 ))
   (yojson ( >= 1.6.0 ))
   (lwt (>= 4.2.0))
   num
   re
   fmt
   logs
   atdgen-runtime
   atdgen ; TODO: find a way to specify as build dependency only, as with the `build` package variable in opam
   kappa-library
   )
)

(package
  (name kappa-webapp)
  (synopsis "Browser app for an interactive use of the Kappa tool suite")
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
