{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, generics-sop, stdenv, template-haskell
      , transformers
      }:
      mkDerivation {
        pname = "exhaustive";
        version = "1.1.2";
        src = ./.;
        libraryHaskellDepends = [
          base generics-sop template-haskell transformers
        ];
        homepage = "http://github.com/ocharles/exhaustive";
        description = "Compile time checks that a computation considers producing data through all possible constructors";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
