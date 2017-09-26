{ mkDerivation, stdenv, array, base, containers, Decimal, either,
    ghcjs-dom, jsaddle, mtl, parsec, reflex, reflex-dom-core, 
    safe, text, time
    , jsaddle-warp, warp, wai-app-static, wai-middleware-static, websockets
    , wai
    , compiler ? "ghc"
    , ghcjs-base 
    , jsaddle-webkit2gtk 
    , reflex-dom-htmlea ? ""
    # , reflex-dom-htmlea ? (import <nixpkgs> {}).haskellPackages.reflex-dom-htmlea
}:
mkDerivation {
    pname = "7guis-reflex";
    version = "0.1.1.0";
    src = ./.;
    isLibrary = true;
    isExecutable = true;

    libraryHaskellDepends = [
      array base containers Decimal either
      ghcjs-dom jsaddle mtl parsec reflex reflex-dom-core 
      safe text time reflex-dom-htmlea
    ]; 
    /* ++ (if compiler == "ghc" */
    /*       then [jsaddle-warp warp wai-app-static wai-middleware-static websockets] */
    /*       else [] */
    /*    ); */
    executableHaskellDepends = [
      array base containers Decimal either
      ghcjs-dom jsaddle mtl parsec reflex reflex-dom-core 
      safe text time reflex-dom-htmlea
    ] ++ (if compiler == "ghc"
            # then [jsaddle-webkit2gtk]
            then [jsaddle-warp warp wai-app-static jsaddle-webkit2gtk
              wai-middleware-static websockets]
            else [ghcjs-base]);
    description = "A reflex-dom API for HTML elements and attributes";
    license = stdenv.lib.licenses.bsd3;
}

