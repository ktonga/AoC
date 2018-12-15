{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, list-zipper, split, stdenv
      }:
      mkDerivation {
        pname = "aoc2018";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base containers list-zipper split ];
        doHaddock = false;
        homepage = "https://github.com/ktonga/aoc2018";
        description = "Advent of Code 2018";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
