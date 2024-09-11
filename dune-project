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
   (ocaml (and ( >= 4.13.0) ( < 5.0.0 )))
   (yojson ( >= 2.0 ))
   (lwt ( >= 4.2.0 ))
   num
   re
   fmt
   logs
   result
   stdlib-shims
   camlp-streams
   ; If adding unpinned libs: consider pinning them in binaries below
   )
)

(package
  (name kappa-binaries)
  (synopsis "Command line interfaces of the Kappa tool suite. Use this package if you want the CLI kappa tools")
  (depends
   (ocaml (and ( >= 4.13.0) ( < 5.0.0 )))
   (yojson ( = 2.1.0 ))
   (lwt ( = 5.7.0 ))
   (num ( = 1.5 ))
   (re ( = 1.11.0 ))
   (fmt ( = 0.9.0 ))
   (logs ( = 0.7.0 ))  
   kappa-library
   ; Added to pin version of lib used in kappa-library
   (result ( = 1.5 ))
   (stdlib-shims ( = 0.3.0 ))
   (camlp-streams ( = 5.0.1 ))
   )
  (depopts
    labltk)
)

(package
  (name kappa-agents)
  (synopsis "Backends for an interactive use of the Kappa tool suite. Used by the kappa-webapp, probably not relevant else. Previously used by deprecated kappa-server")
  (depends
   (ocaml (and ( >= 4.13.0) ( < 5.0.0 )))
   (yojson ( = 2.1.0 ))
   (lwt ( = 5.7.0 ))
   (num ( = 1.5 ))
   (re ( = 1.11.0 ))
   (fmt ( = 0.9.0 ))
   (logs ( = 0.7.0 ))
   (atdgen ( = 2.15.0 )) ; TODO: find a way to specify as build dependency only, as with the `build` package variable in opam
   (atdgen-runtime ( = 2.15.0 ))
   kappa-library
   ; Added to pin version of lib used in kappa-library
   (result ( = 1.5 ))
   (stdlib-shims ( = 0.3.0 ))
   (camlp-streams ( = 5.0.1 ))
   )
)

(package
  (name kappa-webapp)
  (synopsis "Browser app for an interactive use of the Kappa tool suite. Best way to start with the kappa language")
  (depends
   (ocaml (and ( >= 4.13.0) ( < 5.0.0 )))
   (dune ( = 3.14.0 ))
   (yojson ( = 2.1.0 ))
   (lwt ( = 5.7.0 ))
   (lwt_react ( = 1.2.0 ))
   (num ( = 1.5 ))
   (re ( = 1.11.0 ))
   (fmt ( = 0.9.0 ))
   (logs ( = 0.7.0 ))
   (atdgen-runtime ( = 2.15.0 ))
   (atdgen ( = 2.15.0 )) ; TODO: find a way to specify as build dependency only, as with the `build` package variable in opam
   (tyxml-ppx ( = 4.6.0 ))
   (js_of_ocaml ( = 5.7.0 ))
   (js_of_ocaml-lwt ( = 5.7.0 ))
   (js_of_ocaml-ppx ( = 5.7.0 ))
   (js_of_ocaml-tyxml ( = 5.7.0 ))
   kappa-binaries
   kappa-agents
   )
  ; does not build executables as there seem to be no way to build js exes in a package ? TODO: see if we can do something
)
