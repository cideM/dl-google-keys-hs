# https://github.com/Gabriel439/haskell-nix/pull/72
let
  sources = import ./nix/sources.nix;

  nixpkgs = import sources.nixpkgs { };

  app = nixpkgs.haskellPackages.callPackage ./project.nix { };

in
nixpkgs.mkShell {
  inputsFrom = [ app.env ];
  buildInputs = with nixpkgs.haskellPackages; [
    ghcid
    ormolu
    hlint
    cabal2nix
    cabal-install
    cabal-fmt
  ];
}
