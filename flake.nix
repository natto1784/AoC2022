{
  description = "AoC 2022 in Haskell";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/release-22.05;
    utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };
        in
        {
          devShells = with pkgs; rec {
            default = mkShell {
              buildInputs = [ ghc ];
            };
            withLsp = mkShell {
              buildInputs = [
                ghc
                haskell-language-server
              ];
            };
          };
        }
      );
}
