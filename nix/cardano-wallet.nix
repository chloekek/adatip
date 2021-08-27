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
      # Copy over some of the binaries, but not the ones that clash with the
      # ones in cardano-node.
      mkdir -p $out/bin
      for f in "bech32" "cardano-address" "cardano-wallet"; do
        cp $src/$f $out/bin
      done

      mkdir $out/lib
      cp -r $src/auto-completion $out/lib
    '';
  }
