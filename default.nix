{}:
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    guis7-reflex  = ./guis7-reflex;
    guis7-reflex-kit  = ./guis7-reflex-kit;
    guis7-reflex-wai  = ./guis7-reflex-wai;
    guis7-reflex-js  = ./guis7-reflex-js;
  };
  overrides = self: super: {
    reflex-dom-htmlea = self.callCabal2nix "reflex-dom-htmlea"
      # ../reflex-dom-htmlea {};  /* use this when doing things locally */
      (pkgs.fetchFromGitHub {
        owner = "gspia";
        repo = "reflex-dom-htmlea";
        rev = "2900ca4a6840c2362fe56cd9b6cdf7744a646fdd";
        sha256 = "06hjrpnv9314dfdl18a0sm7dxwcx8ps278bfc330h39wb9c7iqi9";
      }) {};
  };
  # android.exampleTbl = {
  #   executableName = "exampleTbl";
  #   applicationId = "org.example.exampleTbl";
  #   displayName = "Example Tables App";
  # };
  /* ios.keyboard = { */
  /*   executableName = "keyboard"; */
  /*   bundleIdentifier = "org.example.keyboard"; */
  /*   bundleName = "Example iOS App (keyboard ex)"; */
  /* }; */

  shells = {
    ghc   = [ "guis7-reflex" "guis7-reflex-wai" "guis7-reflex-kit" ];
    ghcjs = [ "guis7-reflex" "guis7-reflex-js"];
  };
  tools = ghc: with ghc; [
    pkgs.haskellPackages.hlint
    # pkgs.haskellPackages.hasktags
    # pkgs.haskellPackages.haskdogs
    # pkgs.haskellPackages.hdevtools 
    # pkgs.haskellPackages.hindent
    # pkgs.haskellPackages.hsimport
    # pkgs.haskellPackages.pointfree
    # pkgs.haskellPackages.pointful
    # pkgs.haskellPackages.stylish-haskell
  ];
})
