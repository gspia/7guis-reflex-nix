{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc"
} : 
let
  inherit (nixpkgs) pkgs;
  drv = import ./. { inherit compiler; };
  /* f = d: with pkgs; d // { */
  /*     # nativeBuildInputs = d.nativeBuildInputs ++ [ */
  /*     buildInputs = d.buildInputs ++ [ */
  /*       ghc-mod */ 
  /*       hasktags */
  /*       haskdogs */
  /*       hdevtools */
  /*       hlint */ 
  /*       pointfree */
  /*       pointful */
  /*     ]; */
  /*   }; */
  /* g = d: d // { */
  /*     env = f d.env; */
  /*   }; */
  /* drv2 = g drv; */
in
if pkgs.lib.inNixShell then drv.env else drv
