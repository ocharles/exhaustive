{ mkDerivation, base, generics-sop, stdenv, template-haskell
, transformers
}:
mkDerivation {
  pname = "exhaustive";
  version = "1.1.0";
  src = ./.;
  buildDepends = [ base generics-sop template-haskell transformers ];
  homepage = "http://github.com/ocharles/exhaustive";
  description = "Compile time checks that a computation considers producing data through all possible constructors";
  license = stdenv.lib.licenses.bsd3;
}
