{ mkDerivation, attoparsec, base, bifunctors, cabal-install
, hindent, hlint, optparse-applicative, stdenv, text, vector
}:
mkDerivation {
  pname = "smartincludes";
  version = "1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bifunctors optparse-applicative text vector
  ];
  executableToolDepends = [ cabal-install hlint ];
  description = "Reorder C++ include directives, smart";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
