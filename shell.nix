with import <nixpkgs> { };
let
  hsPkgs = haskell.packages.ghc801;
in
  haskell.lib.buildStackProject {
     name = "itmo-haskell-course";
     ghc = hsPkgs.ghc;
     buildInputs = [git ncurses binutils];
     LANG = "en_US.UTF-8";
     TH_ENV = "heh mda nix";
  }
