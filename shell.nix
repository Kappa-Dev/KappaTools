##
## shell.nix - Configuration to set up a build environment for
##             the Kappa tools using nix. Most often this will
##             be used in conjunction with direnv. The .envrc
##             file in this directory contains "use nix" for
##             this purpose. Do "direnv allow" and the build
##             environment will be set up when changing into
##             this directory.
##

with import <nixpkgs> {};
clangStdenv.mkDerivation {
  name = "kappa";
  buildInputs = lib.optionals clangStdenv.isDarwin [
   darwin.apple_sdk.frameworks.CoreServices
  ] ++ [
    gmp.dev
    opam
    pkg-config
  ];
}
