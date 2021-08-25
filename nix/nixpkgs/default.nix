let
    # Read version and hash from pinned.toml file.
    pinned = fromTOML (builtins.readFile ./pinned.toml);
    tarball = fetchTarball pinned;

    # Configuration and overlays for Nixpkgs.
    config = { };
    overlays = [
        (import ../cardano-node)
    ];

in
    import tarball { inherit config overlays; }
