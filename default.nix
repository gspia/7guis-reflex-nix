{ reflex-platform ? import ./reflex-platform.nix
# , reflex-dom-htmlea ? import ./reflex-dom-htmlea.nix
# , reflex-dom-htmlea ? (import <nixpkgs> {}).haskellPackages.reflex-dom-htmlea
, compiler ? "ghcjs"
} :
# , reflex-dom-htmlea ? import ./reflex-dom-htmlea.nix
# Note that default has ghcjs as default compiler while 
# shell.nix has ghc. This way we can use ghcid and some other tools
# while developing (e.g. using the work-on ghc -script).
# nix-build uses default.nix and thus ghcjs.
let
  initialNixpkgs = import <nixpkgs> {};

  /* sources = { */
  /*   reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub { */
  /*     owner  = "reflex-frp"; */
  /*     repo   = "reflex-platform"; */
  /*     rev    = "b7c00b3574d0ef42974eda0f2812c794c7b5d4f3"; */
  /*     sha256 = "1jfz17y2fq051caby4y4aslxrpvgwwa30ivfw0l5wn5pp5zlrpad"; */
  /*   }; */
  /* }; */
  /* reflex-platform = import sources.reflex-platform {}; */

  pkgs  = reflex-platform.nixpkgs.pkgs;
  hpkgs = initialNixpkgs.pkgs.haskellPackages;
  hLib =  initialNixpkgs.haskell.lib;

  indexHtml = ''
    <!DOCTYPE html>
    <html>
      <head>
         <title>7GUIs with Reflex DOM and htmlea</title>
         <link rel="stylesheet" href="index.css">
      </head>
      <body>
      </body>
      <script language="javascript" src="js/7guis-reflex-js.min.js"></script>

    </html>
  '';

  adjust-for-ghcjs = drv: {
    executableSystemDepends = [
      hpkgs.cabal-install
    ];
    executableToolDepends = [pkgs.closurecompiler pkgs.zopfli];
    # extraLibraries = [ reflex-dom-htmlea ];
    # buildDepends = [ reflex-dom-htmlea ];
    # setupHaskellDepends = [ reflex-dom-htmlea ];
    # pkgconfigDepends = [ reflex-dom-htmlea ];
    doHaddock = false;
    postInstall = ''
      mkdir -p $out
      mkdir -p $out/js
      cp $out/bin/7guis-reflex-js.jsexe/all.js $out/js/7guis-reflex-js.js
      cd $out/bin/7guis-reflex-js.jsexe
      closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > $out/js/7guis-reflex-js.min.js
      # rm -Rf $out/bin
     cat <<EOF > $out/index.html
       ${indexHtml}
     EOF
    '';
  };
# Note that if editor makes comments with the /* */, then inside the above shell 
# script (the above '' ''-block) those /**/-blocks are not recognized as comments.
# In nix-lang /* */ work as comments.
  #    rm -Rf $out/bin/short.jsexe
  #    cd $out/js
  #    gzip short.min.js
  # zopfli is gzip on steroids.
  #    zopfli -i1000 short.min.js
  #installPhase = ''
  #  mkdir $out
  #  cp -r ./* $out/
  #'';
  # phase = ["unpackPhase" "buildPhase" "installPhase"];
  adjust-for-ghc = drv: {
    executableSystemDepends = [
      reflex-platform.${compiler}.ghcid
      reflex-platform.${compiler}.cabal-install
      /* hpkgs.ghc-mod */
      /* hpkgs.hasktags */
      /* hpkgs.haskdogs  # stack config set system-ghc --global true */
      /* hpkgs.hdevtools */
      /* hpkgs.hlint */
      /* hpkgs.pointfree */
      /* hpkgs.pointful */
      /* hpkgs.stack */
    ];
    # extraLibraries = [ reflex-dom-htmlea ];
    # buildDepends = [ reflex-dom-htmlea ];
    # setupHaskellDepends = [ reflex-dom-htmlea ];
    # pkgconfigDepends = [ reflex-dom-htmlea ];
    /* executableHaskellDepends = [ */
    /* ]; */
  };

  adjust =
    if compiler == "ghcjs"
    then adjust-for-ghcjs
    else adjust-for-ghc;

  haskellPackages = reflex-platform.${compiler}.override {
    overrides = (self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
      /* reflex-dom-htmlea = hLib.dontHaddock */ 
      /*   (hLib.dontCheck (self.callCabal2nix "reflex-dom-htmlea" ( */
      reflex-dom-htmlea = hLib.dontHaddock 
        (self.callCabal2nix "reflex-dom-htmlea" (
          initialNixpkgs.pkgs.fetchFromGitHub { 
            owner  = "gspia";
            repo   = "reflex-dom-htmlea";
            rev = "502b7f1478a65f643af3391ba8d0df1883872aff";
            sha256 = "1w7qp2kpfp0ivm5xlfafsik21wx7yhh3q7prdn369b1p3zrni5cr";
          }
      ) { } );
    });
  };
  sevenGuis-reflex-code-base = 
    haskellPackages.callPackage ./7guis-reflex.nix { 
      inherit compiler; 
      /* inherit reflex-dom-htmlea; */ 
    };
  sevenGuis-reflex-code = 
   pkgs.haskell.lib.overrideCabal sevenGuis-reflex-code-base adjust;
in
  sevenGuis-reflex-code
