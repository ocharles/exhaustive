let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      exhaustive = self.callPackage ./. {};
    };
  };

in pkgs.runCommand "exhaustive" {
     buildInputs = [
       pkgs.curl
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.hscolour
       ] ++ hs.exhaustive.propagatedNativeBuildInputs)))
     ];
   } ""
