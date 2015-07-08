{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let
  haskellPackageSet = nixpkgs.pkgs.haskell.packages.${compiler};
  haskellEnv = haskellPackageSet.ghcWithPackages (p: with p; [
    cabal2nix
    cabal-install
    hakyll
  ]);

  yiDocEnv = nixpkgs.stdenv.mkDerivation {
    name = "yiDocEnv";
    buildInputs = with nixpkgs.pkgs; [ haskellEnv ];
  };
in

yiDocEnv
