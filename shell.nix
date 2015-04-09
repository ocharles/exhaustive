let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskell-ng.packages.ghc7101.override {
    overrides = self: super: {
      exhaustive = self.callPackage ./. {};
    };
  };

in haskellPackages.exhaustive.env
