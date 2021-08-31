# SPDX-License-Identifier: AGPL-3.0-only

let
    # Read version and hash from pinned.toml file.
    pinned = fromTOML (builtins.readFile ./pinned.toml);
    tarball = fetchTarball pinned;

    # Configuration and overlays for Nixpkgs.
    config = { };
    overlays = [ ];

in
    import tarball { inherit config overlays; }
