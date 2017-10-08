let  
  initialNixpkgs = import <nixpkgs> {};

  sources = {
    reflex-platform = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-platform";
      rev = "02b1e0aac1667cb6e1788d5ca72749e0e0178ae3";
      sha256 = "1kbq58zb69801y60r1v55v836zrib7rcs9ffgz3x8h7kag26xzxw";
    };
  };

  reflex-platform = import sources.reflex-platform {};
in
  reflex-platform

