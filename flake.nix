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
      # linker looks for libCrypt32 but it's actually libcrypt32.
      libcrypt-workaround = pkgs: with pkgs; runCommand "link-crypt32" {} ''
        mkdir -p $out/lib/
        ln -s ${windows.mingw_w64}/lib/libcrypt32.a $out/lib/libCrypt32.a
      '';
      hixProject = {ghc ? "ghc945"}: pkgs.haskell-nix.cabalProject' {
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
          packages.JuicyPixels.components.library.libs =
            lib.optional stdenv.hostPlatform.isWindows windows.mingw_w64_pthreads;
          packages.pandoc-crossref.components.exes.pandoc-crossref.libs =
            lib.optional stdenv.hostPlatform.isWindows (libcrypt-workaround pkgs);
          })];
      };
      overlays = [ haskellNix.overlay ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake_ = args: (hixProject args).flake {
        crossPlatforms = ps: with ps; [ musl64 mingwW64 ];
      };
      flake = flake_ {};
    in {
      packages = {
        default = flake.packages."pandoc-crossref:exe:pandoc-crossref";
        static = flake.packages."x86_64-unknown-linux-musl:pandoc-crossref:exe:pandoc-crossref";
        win = (flake_ { ghc = "ghc928"; })
          .packages."x86_64-w64-mingw32:pandoc-crossref:exe:pandoc-crossref";
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
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "pandoc-crossref.cachix.org-1:LI9ABFTkGpPCTkUTzoopVSSpb1a26RSTJNMsqVbDtPM="
      ];
  };
}
