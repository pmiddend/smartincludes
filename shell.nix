{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, bifunctors, cabal-install
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
        executableToolDepends = [ cabal-install hindent hlint ];
        description = "Reorder C++ include directives, smart";
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
