let
  src = builtins.fetchTarball {
    name = "nixos-unstable";
    url = https://github.com/nixos/nixpkgs/archive/d942688fc137169b577e7bf0c09e01a2ac919b73.tar.gz;
    # git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable
    sha256 = "01072zihcgi0vwld69ffcr08na88703wy6plxp5cb3n1wysfx81g";
  };
  pkgs = import src {};
in pkgs.haskellPackages.callPackage ./production.nix {}
