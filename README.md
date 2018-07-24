# pandoc-crossref filter [![Build Status](https://travis-ci.org/lierdakil/pandoc-crossref.svg?branch=master)](https://travis-ci.org/lierdakil/pandoc-crossref) [![Build status](https://ci.appveyor.com/api/projects/status/v04mfbglpcdqfln4/branch/master?svg=true)](https://ci.appveyor.com/project/lierdakil/pandoc-crossref/branch/master)

pandoc-crossref is a pandoc filter for numbering figures, equations,
tables and cross-references to them.

The input file (like
[demo.md](http://lierdakil.github.io/pandoc-crossref/demo/demo.md)) can
be converted into
[HTML](http://lierdakil.github.io/pandoc-crossref/demo/output.html),
[LaTeX](http://lierdakil.github.io/pandoc-crossref/demo/output.latex),
[PDF](http://lierdakil.github.io/pandoc-crossref/demo/output.pdf),
[Markdown](http://lierdakil.github.io/pandoc-crossref/demo/output.md) or other
formats.

Optionally, you can use cleveref for LaTeX/PDF output, e.g. [cleveref
PDF](http://lierdakil.github.io/pandoc-crossref/demo/output-cref.pdf),
[cleveref
LaTeX](http://lierdakil.github.io/pandoc-crossref/demo/output-cref.latex),
and listings package, e.g. [listings
PDF](http://lierdakil.github.io/pandoc-crossref/demo/output-listings.pdf),
[listings
LaTeX](http://lierdakil.github.io/pandoc-crossref/demo/output-listings.latex)

You can also enable per-chapter numbering (as with `--chapters` for
latex output). You need to specify `-M chapters` for non-LaTeX/PDF
output however. Examples:
[HTML](http://lierdakil.github.io/pandoc-crossref/demo/output-chapters.html),
[Markdown](http://lierdakil.github.io/pandoc-crossref/demo/output-chapters.md),
[LaTeX](http://lierdakil.github.io/pandoc-crossref/demo/output-chapters.latex),
[PDF](http://lierdakil.github.io/pandoc-crossref/demo/output-chapters.pdf).

This work is inspired by
[pandoc-fignos](https://github.com/tomduck/pandoc-fignos) and
[pandoc-eqnos](https://github.com/tomduck/pandoc-eqnos) by @tomduck.

This package tries to use LaTeX labels and references if output type is
LaTeX. It also tries to supplement rudimentary LaTeX configuration that
should mimic metadata configuration by setting `header-includes`
variable.

## Installation

Assuming you already installed [Haskell
platform](http://hackage.haskell.org/platform/), you can install
pandoc-crossref with `cabal`:

``` bash
cabal update
cabal install pandoc-crossref
```

However, I highly recommend you use a sandbox for installation, e.g.

``` bash
cabal update
mkdir pandoc-crossref
cd pandoc-crossref
cabal sandbox init
cabal install pandoc-crossref
```

This will get `pandoc-crossref` installed into `.cabal-sandbox/bin`.
Pandoc will also be built, if it's not installed as a Haskell library
system-wide. You might also want to install `pandoc-citeproc` in the
same sandbox, if that's the case (`cabal install pandoc-citeproc`).

There are a few pre-built executables available at [releases
page](https://github.com/lierdakil/pandoc-crossref/releases/latest) for
Windows, macOS and Linux. Bear in mind that those are a product of an
automated build script, and as such, provided as-is, with zero
guarantees.

Alternatively, you can use a version provided by a third party. At the
time of writing, pandoc-crossref is provided on the following platforms
(that I am aware of):

-   Arch Linux
-   NixOS or Nix package manager (via `nixpkgs.haskellPackages`
    attribute)
-   MacOS (via Homebrew)
-   Gentoo Linux (via gentoo-haskell overlay)

### Notice Fedora users

`cabal-install` package is not enough to build pandoc-crossref (see
\#132). To get sane Haskell build environment, you need to install
`haskell-platform` package (`yum install haskell-platform`).

While on topic, if you don't want to rebuild pandoc itself from source,
make sure you have `pandoc` and `ghc-pandoc-devel` yum packages before
attempting to build pandoc-crossref.

## Usage

Usage information is available at
<https://lierdakil.github.com/pandoc-crossref/>

# License

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

See
[LICENSE](https://github.com/lierdakil/pandoc-crossref/blob/master/LICENSE)
for details.

© 2016 Nikolay Yakimov et al

Contributors (per GPL, holders of copyright on their respective
contributions):

<!-- BEGIN CONTRIBUTORS LIST -->
-   Nikolay Yakimov
-   Raphael Das Gupta
-   Masamichi Hosoda
-   Felix Yan
-   Wlad
-   scoavoux
-   Matthew Salganik
-   Han Joosten
-   Hadrien Mary
-   Chris Black
-   Bart Mesuere

<!-- END CONTRIBUTORS LIST -->
