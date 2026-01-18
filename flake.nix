{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
  };
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages."${system}";
    in
    {
      devShells."${system}".default = pkgs.mkShell {
        packages = [
          pkgs.zlib

          pkgs.haskell.compiler.ghc9122
          pkgs.cabal-install
          (pkgs.haskell-language-server.override { supportedGhcVersions = [ "9122" ]; })
        ];
      };
      formatter."${system}" = pkgs.nixpkgs-fmt;
    };
}
