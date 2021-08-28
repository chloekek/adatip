{ nixpkgs ? import nix/nixpkgs
, profile ? "all" }:

let

    # Configure GHC to know about the Haskell packages we need.
    # Transitive dependencies of packages do not need to be listed.
    # Note that Cabal files must still list the required dependencies.
    ghcWithPackages = nixpkgs.ghc.withPackages haskellPackages;
    haskellPackages = p: [
        p.blaze-html                    # Library for generating HTML.
        p.hasql                         # PostgreSQL client library.
        p.hspec-discover                # Program for finding Haskell tests.
        p.hspec-hedgehog                # Library for generative testing.
        p.optparse-applicative          # Library for parsing CLI arguments.
        p.qrcode-juicypixels            # Library for rendering QR codes.
        p.vector                        # Library for arrays.
        p.warp                          # Library for HTTP servers.
    ];

    cardanoConfig = import ./nix/cardano-config.nix { inherit nixpkgs; };
    cardanoWallet = import ./nix/cardano-wallet.nix { inherit nixpkgs; };

    # By passing '--argstr profile <profile>' to nix-shell
    # you can select a smaller set of packages.
    # This is especially useful in CI
    # to avoid excessive downloads.
    profiles = rec {

        database = [
            nixpkgs.dbmate              # Database schema migration tool.
            nixpkgs.postgresql_13       # Relational database server.
        ];

        dev_env = [
            nixpkgs.entr                # Autoreload tool, to rebuild on change.
            nixpkgs.gitMinimal          # To list files for entr.
            nixpkgs.hivemind            # Process supervisor for dev env.
            nixpkgs.nginx               # Web server and HTTP proxy.
            cardanoWallet
        ];

        haskell = [
            ghcWithPackages             # Haskell compiler.
            nixpkgs.cabal-install       # Haskell build system.
        ];

        ci_tests = haskell;
        ci_db_setup = database;

        all = haskell ++ database ++ dev_env;
    };

in

    # Create a Nix shell environment with all the required development tools.
    nixpkgs.mkShell {

        # Development tools to be made available in the shell.
        nativeBuildInputs = profiles.${profile};

        # Haskell shits itself if it can’t find the UTF-8 locale.
        # Nixpkgs patches glibc to look up locales in LOCALE_ARCHIVE ([1]).
        # [1]: https://nixos.wiki/wiki/Locales
        LOCALE_ARCHIVE = "${nixpkgs.glibcLocales}/lib/locale/locale-archive";

        # Also make sure that the locale is set when using `nix-shell --pure`.
        LANG = "en_US.UTF-8";
        LC_ALL = "en_US.UTF-8";

        # Expose the location of the default Cardano configuration, so we can
        # make it run against the Cardano mainnet or testnet from the Procfile.
        CARDANO_CONFIGURATION = "${cardanoConfig}";

        # The script run-cardano-node.bash, which is started from the Procfile,
        # runs a node that puts its socket here. By setting up this variable,
        # cardano-cli will work out of the box, if you have Hivemind running.
        CARDANO_NODE_SOCKET_PATH = "state/cardano-node.socket";
    }
