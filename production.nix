{ mkDerivation, attoparsec, base, bifunctors, cabal-install
, hindent, hlint, stdenv, text, vector
}:
mkDerivation {
  pname = "smartincludes";
  version = "1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bifunctors text vector
  ];
  executableToolDepends = [ cabal-install hindent hlint ];
  description = "Reorder C++ include directives, smart";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
