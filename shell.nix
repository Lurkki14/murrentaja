{ pkgs ? import <nixpkgs> {} }: with pkgs;
let
  ghc = haskellPackages.ghcWithPackages(p: [ p.text ]);
in
  mkShell {
    nativeBuildInputs = [ghc];
  }

