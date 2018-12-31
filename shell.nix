{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, QuickCheck, stdenv
      , test-framework, test-framework-quickcheck2
      }:
      mkDerivation {
        pname = "grid";
        version = "7.8.12";
        src = ./.;
        libraryHaskellDepends = [ base containers ];
        testHaskellDepends = [
          base QuickCheck test-framework test-framework-quickcheck2
        ];
        homepage = "https://github.com/mhwombat/grid#readme";
        description = "Tools for working with regular grids (graphs, lattices)";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
