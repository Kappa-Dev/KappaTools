# Things you always forget how to achieve

## Profiling (time complexity)

landmarks

## Profiling memory usage
https://github.com/jhjourdan/statmemprof-emacs

diff --git a/core/main/KaSim.ml b/core/main/KaSim.ml
index cf5f9be4b..d28470d70 100644
--- a/core/main/KaSim.ml
+++ b/core/main/KaSim.ml
@@ -137,6 +137,7 @@ let read_interactive_command =
   aux_read_command
 
 let () =
+  let () = Statmemprof_emacs.start 1E-4 30 5 in
   let cli_args = Run_cli_args.default in
   let kasim_args = Kasim_args.default in
   let common_args = Common_args.default in
diff --git a/core/main/dune b/core/main/dune
index 7b4e7fe2f..098cc735a 100644
--- a/core/main/dune
+++ b/core/main/dune
@@ -2,7 +2,8 @@
   (name KaSim)
   (libraries lwt.unix fmt num yojson
              kappa-library.generic kappa-library.mixtures kappa-library.terms kappa-library.runtime
-             kappa_version kappa_grammar kappa_cli)
+             kappa_version kappa_grammar kappa_cli
+            statmemprof-emacs)
   (public_name KaSim)
   (package kappa-binaries)
 (flags (:standard -w @a

## Generate kappy wheels
$ cd dev
$ docker build -t manylinux_opam_2010_x_6_64 .
$ mkdir bidule && chmod 777 bidule
$ docker run -v $PWD/bidule:/io/wheelhouse manylinux_opam_2010_x_6_64 ./build_wheel.sh the_tag_to_build
