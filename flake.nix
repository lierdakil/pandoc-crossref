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
        linker-workaround = pkgs: pkgs.writeShellScript "linker-workaround" ''
          # put all flags into 'params' array
          source ${pkgs.stdenv.cc}/nix-support/utils.bash
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
        hixProject = pkgs.haskell-nix.cabalProject {
          compiler-nix-name = "ghc945";
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
          evalSystem = "x86_64-linux";
          modules = [({pkgs, ...}: with pkgs; {
            packages.pandoc-crossref.ghcOptions =
              lib.optional stdenv.hostPlatform.isMusl "-pgml=${linker-workaround pkgs}";
          })];
        };
        overlays = [ haskellNix.overlay ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = hixProject.flake {
          crossPlatforms = ps: with ps; [ musl64 ];
        };
      in pkgs.lib.recursiveUpdate { inherit (flake) packages apps checks; } {
        packages = {
          default = flake.packages."pandoc-crossref:exe:pandoc-crossref";
          static = flake.packages."x86_64-unknown-linux-musl:pandoc-crossref:exe:pandoc-crossref";
          pandoc = hixProject.pandoc-cli.components.exes.pandoc;
        };
        apps = {
          default = flake.apps."pandoc-crossref:exe:pandoc-crossref";
          test = flake.apps."pandoc-crossref:test:test-pandoc-crossref";
          test-integrative = flake.apps."pandoc-crossref:test:test-integrative";
        };
        devShells.default = pkgs.mkShell {
          buildInputs = with self.packages.${system}; [ default pandoc ];
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
