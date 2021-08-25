# This overlay provides cardano-node [1].
# It just downloads the executables because that is easy.
# The alternative is to use IOHK’s Nix stuff, maybe later.
# [1]: https://github.com/input-output-hk/cardano-node

super: self:

let
    # Read version and hash from pinned.toml file.
    pinned = fromTOML (builtins.readFile ./pinned.toml);
    tarball = fetchTarball pinned;

    # Create a bin directory with the executables in it.
    # The tarball contains the executable in its root.
    cardano-node =
        super.runCommandNoCC "" {} ''
            mkdir --parents "$out"/bin
            find            \
                ${tarball}  \
                -type f     \
                -executable \
                -mindepth 1 \
                -maxdepth 1 \
                -exec ln --symbolic {} "$out"/bin \;
        '';

in
    {
        inherit cardano-node;
    }
