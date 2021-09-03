# SPDX-License-Identifier: AGPL-3.0-only

{ nixpkgs ? import nix/nixpkgs
, env ? "dev" }:

let

    cardanoConfig = import ./nix/cardano-config.nix { inherit nixpkgs; };
    cardanoWallet = import ./nix/cardano-wallet.nix { inherit nixpkgs; };

    # Configure GHC to know about the Haskell packages we need.
    # Transitive dependencies of packages do not need to be listed.
    # Note that Cabal files must still list the required dependencies.
    ghcWithPackages = nixpkgs.ghc.withPackages haskellPackages;
    haskellPackages = p: [
        p.aeson                         # Dependency of cardano-wallet client.
        p.blaze-html                    # Library for generating HTML.
        p.hasql                         # PostgreSQL client library.
        p.hspec-discover                # Program for finding Haskell tests.
        p.hspec-hedgehog                # Library for generative testing.
        p.http-client                   # HTTP client library.
        p.mtl                           # Monad* classes.
        p.optparse-applicative          # Library for parsing CLI arguments.
        p.qrcode-juicypixels            # Library for rendering QR codes.
        p.temporary                     # Library for temporary files.
        p.transformers                  # Monad transformers.
        p.vector                        # Library for arrays.
        p.warp                          # Library for HTTP servers.
    ];

    # Based on the selected environment we will export different variables.
    # For instance, when testing, there is no “one global database”
    # so it wouldn’t make sense to set the PG* variables in that case.
    exportedVariables = rec {

        # These are common to all environments.
        common = {

            # Sanitize the locale settings so that Haskell works properly.
            # Nixpkgs patches glibc to look up locales in LOCALE_ARCHIVE ([1]).
            # [1]: https://nixos.wiki/wiki/Locales
            LOCALE_ARCHIVE = "${nixpkgs.glibcLocales}/lib/locale/locale-archive";
            LANG = "en_US.UTF-8";
            LC_ALL = "en_US.UTF-8";

            # Expose the location of the default Cardano configuration,
            # so we can make it run against the Cardano mainnet or testnet
            # from the Procfile.
            CARDANO_CONFIGURATION = "${cardanoConfig}";

        };

        # These are meant for environments that assume a single PostgreSQL.
        common-db = rec {

            # PostgreSQL env variables used by all sorts of tools.
            # Note that the database setup scripts will override these
            # with their own values as they need to authenticate differently.
            PGDATA = toString state/postgresql/pgdata;
            PGHOST = "127.0.0.1";
            PGPORT = "8082";
            PGUSER = "adatip_setup";
            PGPASSWORD = PGUSER;
            PGDATABASE = "adatip";

            # Configure dbmate for the same reason.
            DATABASE_URL = "postgres://${PGHOST}:${PGPORT}/?sslmode=disable";
            DBMATE_MIGRATIONS_DIR = toString ./database;
            DBMATE_NO_DUMP_SCHEMA = "true";

        };

        # These are meant for environments that assume a single Cardano node.
        common-cardano = {

            # The script run-cardano-node.bash,
            # which is started from the Procfile,
            # runs a node that puts its socket here.
            # cardano-cli also uses this variable for finding the socket,
            # so it will work out of the box if Hivemind is running.
            CARDANO_NODE_SOCKET_PATH = toString state/cardano-node.socket;

        };

        # These are the environments you can choose.
        dev = test // common-db // common-cardano;
        test = common;
        ci-test = test;
        ci-db-setup = common // common-db;

    }."${env}";

    # Programs and libraries that must be available in the shell.
    nativeBuildInputs = [
        cardanoWallet               # Watches Cardano address for transactions.
        ghcWithPackages             # Haskell compiler.
        nixpkgs.cabal-install       # Haskell build system.
        nixpkgs.dbmate              # Database schema migration tool.
        nixpkgs.entr                # Autoreload tool, to rebuild on change.
        nixpkgs.hivemind            # Process supervisor for dev env.
        nixpkgs.nginx               # Web server and HTTP proxy.
        nixpkgs.postgresql_13       # Relational database server.
        nixpkgs.python3             # To check migration file names.
        nixpkgs.sassc               # Macro processor for CSS.
    ];

in

    nixpkgs.mkShell (
        { inherit nativeBuildInputs; }
        // exportedVariables
    )
