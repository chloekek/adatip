{ nixpkgs ? import ./nixpkgs }:

nixpkgs.stdenv.mkDerivation {
  name = "cardano";
  version = "1.27.0-linux";

  src = builtins.fetchTarball {
    url = "https://hydra.iohk.io/build/6263009/download/1/cardano-node-1.27.0-linux.tar.gz";
    sha256 = "06kw6jnddvf5c8dvvlf748igyr8m1f772vz7hl2yfhd7k5d2jn89";
  };

  phases = [ "installPhase" ];

  installPhase = ''
    # Copy all files in the root of the archive, the binaries.
    mkdir -p $out/bin
    find $src -maxdepth 1 -type f -exec cp '{}' $out/bin ';'

    # Copy the network configuration to the lib output.
    mkdir -p $out/lib
    cp -r $src/configuration $out/lib
  '';
}
