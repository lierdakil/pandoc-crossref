# pandoc-crossref filter [![Build Status](https://travis-ci.org/lierdakil/pandoc-crossref.svg?branch=master)](https://travis-ci.org/lierdakil/pandoc-crossref) [![Build status](https://ci.appveyor.com/api/projects/status/v04mfbglpcdqfln4/branch/master?svg=true)](https://ci.appveyor.com/project/lierdakil/pandoc-crossref/branch/master)

pandoc-crossref is a pandoc filter for numbering figures, equations,
tables and cross-references to them.

The input file (like
[demo.md](https://raw.githubusercontent.com/lierdakil/pandoc-crossref/master/docs/demo/demo.md)) can
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

The easiest option to get pandoc-crossref on Windows, macOS, or Linux, is to
download pre-built executables available at the [releases
page](https://github.com/lierdakil/pandoc-crossref/releases/latest).
Bear in mind that those are a product of automated build scripts, and as
such, provided as-is, with zero guarantees. Feel free to open issues if those
don't work though, I'll try to do what I can.

**WARNING:** When using pre-built executables, **make sure that your pandoc
version matches the version pandoc-crossref was built against, otherwise
weird stuff will likely happen.** Feel free to open issues if there's a new
version of pandoc available, for which there are no pandoc-crossref builds.

Also, for those feeling adventurous, the automatic builds for the latest commits are available. Latest Windows builds can be found on [AppVeyor build history page](https://ci.appveyor.com/project/lierdakil/pandoc-crossref/history) (select a build and go to Artifacts). Latest Linux and MacOS builds can be found on [BinTray]( https://bintray.com/lierdakil/pandoc-crossref/pandoc-crossref/nightly/view/files?sort=name&order=asc#files/).

If you don't trust random binaries downloaded off the Internet (which is
completely reasonable), you're welcome to build from source. You have two
preferred options for that: building from Hackage with `cabal-install`
(you'll need [Haskell platform][]), or from repository with `stack` (you'll
only need `stack` and maybe `git`). See below for build instructions.

If you're completely new to Haskell, the latter, i.e. building from repo
with `stack`, is the easier option in most cases.

[Haskell platform]: http://hackage.haskell.org/platform/

Alternatively, you can use a version provided by a third party. At the
time of writing, pandoc-crossref is provided on the following platforms
(that I am aware of):

-   Arch Linux
-   NixOS or Nix package manager (via `nixpkgs.haskellPackages`
    attribute)
-   MacOS (via Homebrew)
-   Gentoo Linux (via gentoo-haskell overlay)

### Building from Hackage with `cabal-install` and Haskell platform

Assuming you already installed [Haskell platform][] by whatever means necessary, you can install pandoc-crossref with `cabal`.

If you have `cabal-install` version 2.4 or newer (i.e. `cabal --version` shows `2.4.x.x`), **and** you are not on Windows, I recommend using new-style install (new-style install is regrettably broken on Windows at the time of writing):

``` bash
cabal new-update
cabal new-install pandoc pandoc-crossref pandoc-citeproc
```

This will get `pandoc-crossref`, `pandoc` and `pandoc-citeproc` executables symlinked to `$HOME/.cabal/bin`, which you can then add to `PATH` or copy/move the symlinks where you want them.

On cabal-install version 2.2, it's possible to do the same, albeit you'll need to use `cabal update` instead of `cabal new-update`.

On older cabal-install versions that don't support new-style installs (or on Windows), I highly recommend you use a sandbox for installation, e.g.

``` bash
cabal update
mkdir pandoc-crossref
cd pandoc-crossref
cabal sandbox init
cabal install pandoc pandoc-crossref pandoc-citeproc
```

This will get `pandoc`, `pandoc-citeproc`, and `pandoc-crossref` installed into `.cabal-sandbox/bin`.

Refer to cabal documentation if you need to build a particular version (TL;DR: add `--constraint pandoc-crossref==<version>` to the installation command)

### Building from repo with `stack`

If you want to build an unreleased version, just fancy building from repo, or don't want to install the Haskell platform, you can clone the repository, check out the commit/tag/branch you want and build with `stack`.

First of all, get `stack` if you don't have it already: see the [official stack documentation][]. Note that `stack` is also included in the [Haskell platform][], and on Linux it is usually available in your package manager.

[official stack documentation]: https://docs.haskellstack.org/en/stable/README/#how-to-install

If you have `git`, you can now clone the repository and build:

``` bash
git clone https://github.com/lierdakil/pandoc-crossref.git
cd pandoc-crossref
git checkout <commit/tag/branch>
stack install
```

If you don't have `git`, just download the sources for your preferred commit/branch/tag via the GitHub interface, and run `stack install` in the directory that contains `stack.yaml` file.

This will install pandoc-crossef executable to `$HOME/.local/bin`. You might also want to separately run `stack install pandoc pandoc-citeproc` in the same directory (i.e. the root of the repository, the one containing `stack.yaml` file)

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
-   Wandmalfarbe
-   scoavoux
-   Salim B
-   Matthew Salganik
-   Han Joosten
-   Hadrien Mary
-   Chris Black
-   Bart Mesuere

<!-- END CONTRIBUTORS LIST -->
