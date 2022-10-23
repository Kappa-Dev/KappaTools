##
## shell.nix - Configuration to set up a build environment for
##             the Kappa tools using nix. Most often this will
##             be used in conjunction with direnv. The .envrc
##             file in this directory contains "use nix" for
##             this purpose. Do "direnv allow" and the build
##             environment will be set up when changing into
##             this directory.
##

with import <nixpkgs> {
##  Building on Apple computers with ARM chips fails. Set the
##  system type to x86_64 and use Rosetta by uncommenting the
##  following line.
#
#  system = "x86_64-darwin";
};
clangStdenv.mkDerivation {
  name = "kappa";
  buildInputs = [
    gmp.dev
    opam
    pkg-config
  ];
}
