{
  inputs = {
    cardanoNode.url = "github:input-output-hk/cardano-node?tag=1.28.0";
    nixpkgs.url = "github:nixos/nixpkgs?rev=bad3ccd099ebe9a8aa017bda8500ab02787d90aa";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    cardanoNode,
    nixpkgs,
    self,
    utils,
  }:
  utils.lib.eachSystem ["x86_64-linux"]
    (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        rec {
          devShell = (import ./shell.nix) { nixpkgs = pkgs; };
          packages = {
            cardano-node = cardanoNode.apps.${system}.cardano-node;
            cardano-cli = cardanoNode.apps.${system}.cardano-cli;
          };
        }
    );
}
