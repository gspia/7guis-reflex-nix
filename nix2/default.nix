{ nixpkgsOld ? import <nixpkgs> {}
, compiler ? "ghcjs" 
}:
let
# This is modified from intro-to-reflex-talk/code/default.nix
  # reflex-platform = import (nixpkgsOld.pkgs.fetchFromGitHub {
  #   owner = "reflex-frp";
  #   rev  = "32833c010ecf868826aaa3b60d322bf697f37134";
  #   sha256 = "1n34kqlfaixsxj6zknif6vzv1fqysnnwg9vdx4mgi3515cqv0jsp";
  #   repo = "reflex-platform";
  # }) {} ;
  reflex-platform = import (nixpkgsOld.pkgs.fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "b7c00b3574d0ef42974eda0f2812c794c7b5d4f3";
    sha256 = "1jfz17y2fq051caby4y4aslxrpvgwwa30ivfw0l5wn5pp5zlrpad";
  }) {} ;



  nixpkgs = reflex-platform.nixpkgs;
  pkgs = nixpkgs.pkgs;
  fixUp = if compiler == "ghcjs" then pkgs.haskell.lib.dontHaddock else nixpkgs.lib.trivial.id;

  reflex-7g-code-base = fixUp (reflex-platform.${compiler}.callPackage ./7guis-reflex.nix {});
  reflex-7g-code = pkgs.haskell.lib.overrideCabal reflex-7g-code-base (drv: {
  executableToolDepends = [];
#  executableToolDepends = [pkgs.closurecompiler pkgs.zopfli];
#  postInstall = ''
#    mkdir -p $out
#
#    mkdir -p $out/css
#    cp ./css/* $out/css/
#
#    mkdir -p $out/js
#    cp $out/bin/examples-exe.jsexe/all.js $out/js/examples.js
#
#    cd $out/bin/examples-exe.jsexe
#    closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_functi
#on_wrapper --jscomp_off="*" --externs=all.js.externs > $out/js/examples.min.js
#    rm -Rf $out/bin/examples-exe.jsexe
#    rm -Rf $out/bin
#
#    cd $out/js
#    zopfli -i1000 examples.min.js
#
#    rm -Rf $out/lib
#    rm -Rf $out/nix-support
#    rm -Rf $out/share
#  '';
});
# nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./7guis-reflex.nix { }
in
  reflex-7g-code
