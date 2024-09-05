; Windows build only has access to 4.0.0, where --disable flags don't exist yet in jsoo
; TODO: better distinguish this, as this should depend on jsoo version and not OS

(rule
 (with-stdout-to
  js_of_ocaml_flags.Unix
  (echo "(:standard --disable globaldeadcode --disable deadcode)")))

(rule
 (with-stdout-to
  js_of_ocaml_flags.Win32
  (echo ":standard")))
