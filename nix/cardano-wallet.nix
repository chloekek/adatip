{ nixpkgs ? import ./nixpkgs }:
  nixpkgs.stdenv.mkDerivation {
    name = "cardano-wallet";
    version = "v2021-08-27-linux64";

    src = builtins.fetchTarball {
      url = "https://hydra.iohk.io/build/6275166/download/1/cardano-wallet-v2021-04-28-linux64.tar.gz";
      sha256 = "18m1nrbdgcy9d2rivhr7wyngv37crx0zkqibynb2j5mfbs2hz515";
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
