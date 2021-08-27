{ nixpkgs ? import ./nixpkgs }:
let
  # Cardano config files for mainnet and devnet are hosted at
  # https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/index.html,
  # but as separate files, so we need to jump through a lot of hoops to get all
  # of them.
  fetchConfig = { name, sha256 }: builtins.fetchurl {
    url = "https://hydra.iohk.io/build/7370192/download/1/${name}";
    inherit sha256;
  };

  # A function that produces a line of Bash that copies the given file.
  cpConfig = { name, sha256 }:
    let
      srcFile = fetchConfig { inherit name sha256; };
    in
      "cp ${srcFile} $out/${name}";

  mkConfig = { name, configSha256, topologySha256, byronSha256, shelleySha256, alonzoSha256 }:
    builtins.concatStringsSep "\n" [
      (cpConfig { name = "${name}-config.json";          sha256 = configSha256; })
      (cpConfig { name = "${name}-topology.json";        sha256 = topologySha256; })
      (cpConfig { name = "${name}-byron-genesis.json";   sha256 = byronSha256; })
      (cpConfig { name = "${name}-shelley-genesis.json"; sha256 = shelleySha256; })
      (cpConfig { name = "${name}-alonzo-genesis.json";  sha256 = alonzoSha256; })
  ];

  mainnetConfig = mkConfig {
    name = "mainnet";
    configSha256   = "0mpzayg6i82w9grvwy6p1kb4ql2xrnlml1wp5a2f10pvhakjwy8g";
    topologySha256 = "0c2p6vznyl96l2f1f5phkhdwckvy3d8515apgpl744jxym7iihks";
    byronSha256    = "1ahkdhqh07096law629r1d5jf6jz795rcw6c4vpgdi5j6ysb6a2g";
    shelleySha256  = "0qb9qgpgckgz8g8wg3aa9vgapym8cih378qc0b2jnyfxqqr3kkar";
    alonzoSha256   = "0234ck3x5485h308qx00kyas318dxi3rmxcbksh9yn0iwfpvycvk";
  };

  testnetConfig = mkConfig {
    name = "testnet";
    configSha256   = "1qzhhxa5a5dlqkxwzkxhabgvqsrj5lhl7vn8isqjw3gpf5rn049n";
    topologySha256 = "0kqm5dzl4iynabzsn9br2gdsiqy3wc9cp3iga6knwr9d3ndr3kyb";
    byronSha256    = "11vxckfnsz174slr7pmb5kqpy8bizkrqdwgmxyzl7fbvj2g178yw";
    shelleySha256  = "19ng3grvz3niashggh0vblf5hw2symp34l4j5d25r7diyz8rlc2f";
    alonzoSha256   = "0234ck3x5485h308qx00kyas318dxi3rmxcbksh9yn0iwfpvycvk";
  };
in
  nixpkgs.stdenv.mkDerivation {
    name = "cardano-config";
    version = "2021-08-27";

    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out
      ${mainnetConfig}
      ${testnetConfig}
    '';
  }
