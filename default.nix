let
  sources = import ./nix/sources.nix;

  nixpkgs = import sources.nixpkgs { };
in
{
  api = nixpkgs.haskellPackages.callPackage ./project.nix {};
}
