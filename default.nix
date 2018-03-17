{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, data-accessor
      , data-accessor-template, data-accessor-transformers, data-default
      , deepseq, directory, filepath, hspec, mtl, open-browser
      , optparse-applicative, pandoc, pandoc-types, roman-numerals
      , stdenv, syb, template-haskell, temporary, text, utility-ht
      }:
      mkDerivation {
        pname = "pandoc-crossref";
        version = "0.3.0.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        enableSeparateDataOutput = true;
        libraryHaskellDepends = [
          base containers data-accessor data-accessor-template
          data-accessor-transformers data-default directory filepath mtl
          pandoc pandoc-types roman-numerals syb template-haskell text
          utility-ht
        ];
        executableHaskellDepends = [
          base containers data-accessor data-accessor-template
          data-accessor-transformers data-default deepseq directory filepath
          mtl open-browser optparse-applicative pandoc pandoc-types
          roman-numerals syb template-haskell temporary text utility-ht
        ];
        testHaskellDepends = [
          base containers data-accessor data-accessor-template
          data-accessor-transformers data-default directory filepath hspec
          mtl pandoc pandoc-types roman-numerals syb template-haskell text
          utility-ht
        ];
        homepage = "https://github.com/lierdakil/pandoc-crossref#readme";
        description = "Pandoc filter for cross-references";
        license = "GPL";
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
