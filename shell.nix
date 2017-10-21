{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, bytestring, case-insensitive
      , doctest, hspec, QuickCheck, quickcheck-instances, stdenv, text
      }:
      mkDerivation {
        pname = "http-types";
        version = "0.10";
        src = ./.;
        libraryHaskellDepends = [
          array base bytestring case-insensitive text
        ];
        testHaskellDepends = [
          base bytestring doctest hspec QuickCheck quickcheck-instances text
        ];
        homepage = "https://github.com/aristidb/http-types";
        description = "Generic HTTP types for Haskell (for both client and server code)";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
