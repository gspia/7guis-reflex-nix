{ mkDerivation, array, base, containers, Decimal, either, ghcjs-dom
, jsaddle, jsaddle-webkit2gtk, mtl, parsec, reflex, reflex-dom-core
, safe, stdenv, text, time
#, file-embed
}:
mkDerivation {
  pname = "7guis-reflex";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    array base containers Decimal either ghcjs-dom jsaddle
    jsaddle-webkit2gtk mtl parsec reflex reflex-dom-core safe text time
    # file-embed
  ];
  license = stdenv.lib.licenses.bsd3;
}
