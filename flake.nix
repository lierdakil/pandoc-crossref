{
  # This is a template created by `hix init`
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      linker-workaround = pkgs: pkgs.writeShellScript "linker-workaround" ''
        # put all flags into 'params' array
        source ${pkgs.stdenv.cc}/nix-support/utils.bash
        expandResponseParams "$@"

        # check if '-shared' flag is present
        printf '%s\0' "''${params[@]}" | grep -qFxze '-shared'
        hasShared=$?

        if [[ "$hasShared" -eq 0 ]]; then
          exec "$CC" @<(printf '%q\n' "''${params[@]}" | grep -vFxe '-static')
        else
          # if '-shared' is not set, don't modify the params
          exec "$CC" @<(printf '%q\n' "''${params[@]}")
        fi
      '';
      # a very convoluted way to extract `resolver: ghc-X.Y.Z` from ./stack.yaml
      # and convert it to `ghcXYZ`
      ghc-version = pkgs.lib.trivial.pipe (builtins.readFile ./stack.yaml) [
        (builtins.split "\n")
        (builtins.filter (s: builtins.isString s && pkgs.lib.strings.hasPrefix "resolver:" s))
        (pkgs.lib.trivial.flip builtins.elemAt 0)
        (builtins.split ": ")
        (pkgs.lib.trivial.flip builtins.elemAt 2)
        (pkgs.lib.strings.stringAsChars (c: if builtins.elem c [ "-" "." ] then "" else c))
      ];
      hixProject = {ghc ? ghc-version}: pkgs.haskell-nix.cabalProject' {
        compiler-nix-name = ghc;
        src = nix-filter.lib {
          root = ./.;
          include = [
            ./docs
            ./lib
            ./lib-internal
            ./src
            ./test
            ./pandoc-crossref.cabal
            ./cabal.project.freeze
            ./cabal.project
            ./Setup.hs
            ./.gitignore
            ./LICENSE
            ./README.md
            ./CHANGELOG.md
          ];
        };
        modules = [({pkgs, ...}: with pkgs; {
          packages.pandoc-crossref.ghcOptions =
            lib.optional stdenv.hostPlatform.isMusl "-pgml=${linker-workaround pkgs}";
          })];
      };
      overlays = [ haskellNix.overlay ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake_ = args: (hixProject args).flake {
        crossPlatforms = ps: with ps; [ musl64 ];
      };
      flake = flake_ {};
    in {
      packages = {
        default = flake.packages."pandoc-crossref:exe:pandoc-crossref";
        static = flake.packages."x86_64-unknown-linux-musl:pandoc-crossref:exe:pandoc-crossref";
        pandoc = (hixProject {}).hsPkgs.pandoc-cli.components.exes.pandoc;
        pandoc-with-crossref = pkgs.symlinkJoin {
          name = "pandoc-with-crossref";
          paths = with self.packages.${system}; [ default pandoc ];
        };
      };
      apps = {
        default = flake.apps."pandoc-crossref:exe:pandoc-crossref";
        test = flake.apps."pandoc-crossref:test:test-pandoc-crossref";
        test-integrative = flake.apps."pandoc-crossref:test:test-integrative";
      };
      devShells.default = pkgs.mkShell {
        buildInputs = [ self.packages.${system}.pandoc-with-crossref ];
      };
    });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://pandoc-crossref.cachix.org"
      "https://nix-cache.undo.it/pandoc-crossref"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "pandoc-crossref.cachix.org-1:LI9ABFTkGpPCTkUTzoopVSSpb1a26RSTJNMsqVbDtPM="
      "pandoc-crossref:LUq5vqxFmYJStwKcNstjjGFoGd3+k+jpfyXR2V/eZjw="
      ];
  };
}
