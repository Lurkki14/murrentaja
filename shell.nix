{ pkgs ? import <nixpkgs> {} }: with pkgs;
let
  ghc = haskellPackages.ghcWithPackages(p: with p; [ text optparse-applicative ]);
in
  mkShell {
    nativeBuildInputs = [ghc];
  }

