{
  # This is a template created by `hix init`
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nix-filter.url = "github:numtide/nix-filter";
  outputs = { self, nixpkgs, flake-utils, haskellNix, nix-filter }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        linker-workaround = pkgs_.writeShellScript "linker-workaround" ''
          # put all flags into 'params' array
          source ${pkgsStatic.stdenv.cc}/nix-support/utils.bash
          expandResponseParams "$@"

          # check if '-shared' flag is present
          hasShared=0
          for param in "''${params[@]}"; do
            if [[ "$param" == "-shared" ]]; then
              hasShared=1
            fi
          done

          if [[ "$hasShared" -eq 0 ]]; then
            # if '-shared' is not set, don't modify the params
            newParams=( "''${params[@]}" )
          else
            # if '-shared' is present, remove '-static' flag
            newParams=()
            for param in "''${params[@]}"; do
              if [[ ("$param" != "-static") ]]; then
                newParams+=( "$param" )
              fi
            done
          fi

          # invoke the actual linker with the new params
          exec x86_64-unknown-linux-musl-cc @<(printf "%q\n" "''${newParams[@]}")
        '';
        hixProject = args@{ static ? false, ... }:
          (pkgs args).haskell-nix.cabalProject {
            compiler-nix-name = "ghc927";
            src = nix-filter.lib {
              root = ./.;
              include = [
                ./docs
                ./lib
                ./lib-internal
                ./src
                ./test
                ./pandoc-crossref.cabal
                ./cabal.project
                ./Setup.hs
                ./.gitignore
                ./LICENSE
                ./README.md
                ./CHANGELOG.md
              ];
            };
            evalSystem = "x86_64-linux";
            modules = [{
              packages.pandoc-crossref.ghcOptions =
                if static then ["-pgml=${linker-workaround}"] else [];
            }];
          };
        overlays = [ haskellNix.overlay (final: prev: {}) ];
        pkgsStatic = pkgs_.pkgsCross.musl64;
        pkgs_ = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        pkgs = {static ? false }: if static then pkgsStatic else pkgs_;
        flake = (hixProject {}).flake {};
        flakeStatic = (hixProject { static = true; }).flake {};
      in {
        legacyPackages = pkgs {};
        packages = {
          default = flake.packages."pandoc-crossref:exe:pandoc-crossref";
          static = flakeStatic.packages."pandoc-crossref:exe:pandoc-crossref";
          pandoc = (hixProject {}).pandoc-cli.components.exes.pandoc;
        };
        apps = {
          default = flake.apps."pandoc-crossref:exe:pandoc-crossref";
          test = flake.apps."pandoc-crossref:test:test-pandoc-crossref";
          test-integrative = flake.apps."pandoc-crossref:test:test-integrative";
        };
        devShells.default = pkgs_.mkShell {
          buildInputs = [
            self.packages.${system}.default
            self.packages.${system}.pandoc
          ];
        };
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = ["https://cache.iog.io" "https://pandoc-crossref.cachix.org"];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "pandoc-crossref.cachix.org-1:LI9ABFTkGpPCTkUTzoopVSSpb1a26RSTJNMsqVbDtPM="
      ];
  };
}
