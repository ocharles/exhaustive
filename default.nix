{ cabal, genericsSop, singletons }:

cabal.mkDerivation (self: {
  pname = "exhaustive";
  version = "1.0.0";
  src = ./.;
  buildDepends = [ genericsSop singletons ];
})
