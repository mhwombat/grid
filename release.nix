let
  pkgs = import <nixpkgs> { };

in
  { project = pkgs.haskellPackages.callPackage ./project.nix { };
  }
