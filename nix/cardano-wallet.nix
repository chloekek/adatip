{ nixpkgs ? import ./nixpkgs }:
  nixpkgs.stdenv.mkDerivation {
    name = "cardano-wallet";
    version = "v2021-08-27-linux64";

    src = builtins.fetchTarball {
      url = "https://hydra.iohk.io/build/7422334/download/1/cardano-wallet-v2021-08-27-linux64.tar.gz";
      sha256 = "1x38n6nap4lcwpx3bl6lz83p2kns3drwxkq0vgkwihnfggr49izp";
    };

    phases = [ "installPhase" ];

    installPhase = ''
      # Copy all files in the root of the archive, the binaries.
      mkdir -p $out/bin
      find $src -maxdepth 1 -type f -exec cp '{}' $out/bin ';'

      mkdir $out/lib
      cp -r $src/auto-completion $out/lib
    '';
  }
