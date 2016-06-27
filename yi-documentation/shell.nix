{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellEnv = nixpkgs.haskellPackages.ghcWithPackages (p: with p; [
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
