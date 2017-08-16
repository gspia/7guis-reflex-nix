{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc" 
}:
let
  # inherit (nixpkgs) pkgs compiler;
  inherit (nixpkgs) pkgs;
  drv = import ./. { inherit compiler; };
in 
  if pkgs.lib.inNixShell then drv.env else drv
# (import ./default.nix { inherit nixpkgs compiler; }).env
