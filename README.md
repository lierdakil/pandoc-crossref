# pandoc-crossref filter ![Build status](https://github.com/lierdakil/pandoc-crossref/workflows/Haskell%20CI/badge.svg)

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

**NOTE:** Linux and Windows binaries are packed with `upx` (not macOS though, since upx apparently has questionable interactions with Apple's x86 emulation on A1 processors). If you don't like the overhead, and don't mind 40-megabyte binaries, you can unpack those manually with `upx -d pandoc-crossref`. Also please notice that upx-packed binaries can break in some exotic environments, like empty chroot with no access to `/proc`, etc.

Also, for those feeling adventurous, the automatic builds for the latest commits are available. Latest builds can be found on the [nightlies tag](https://github.com/lierdakil/pandoc-crossref/releases/tag/nightlies) (despite the name, those aren't actually built nightly, but on each push instead)

If you don't trust random binaries downloaded off the Internet (which is
completely reasonable), you're welcome to build from source. You have two
preferred options for that: building from Hackage with `cabal-install`, or from
repository with `stack` (you'll only need `stack` and maybe `git`). See below
for build instructions.

If you're completely new to Haskell, the latter, i.e. building from repo
with `stack`, is the easier option in most cases.

This repository is also a nix flake. You can use `nix` to get it installed.

Alternatively, you can use a version provided by a third party. At the
time of writing, pandoc-crossref is provided on the following platforms
(that I am aware of):

-   Arch Linux
-   NixOS or Nix package manager (via `nixpkgs.haskellPackages`
    attribute)
-   MacOS (via [Homebrew](https://brew.sh))
-   FreeBSD official binary package [textproc/hs-pandoc-crossref](https://www.freshports.org/textproc/hs-pandoc-crossref/)
-   Any Linux distribution (via [Linuxbrew](https://docs.brew.sh/Linuxbrew))
-   Gentoo Linux (via gentoo-haskell overlay)
-   Windows (via [scoop](https://scoop.sh/))

### Building from Hackage with `cabal-install`

You'll need to get GHC and `cabal-install` installed first. By far the easiest way to get those is via [ghcup].

[ghcup]: https://www.haskell.org/ghcup/

Describing using `ghcup` is out of scope for this small guide, but TL;DR is this:

```
ghcup install ghc
ghcup install cabal
```

After you got `cabal-install` and `ghc`, run:

``` bash
cabal v2-update
cabal v2-install --install-method=copy pandoc-cli pandoc-crossref
```

This will get `pandoc-crossref` and `pandoc` executables copied to `$HOME/.cabal/bin` (by default, if not, check your cabal config file `installdir` setting -- find out where your config file is by running `cabal help user-config`), which you can then add to `PATH` or copy/move the symlinks where you want them.

Refer to cabal documentation if you need to build a particular version (TL;DR: add `--constraint pandoc-crossref==<version>` to the installation command)

**Note**: if you're using cabal to build from a repo checkout, and not from Hackage as described above, you'll need to either **match the compiler version** specified in `ghcver` in `.github/workflows/haskell.yml`, or **remove `cabal.project.freeze`** from the root of the repository. Otherwise, cabal will complain about version mismatch of boot packages (like `base`, `ghc-boot-th`, etc)

### Building from repo with `stack`

First of all, get `stack` if you don't have it already: see the [official stack documentation][]. Note that `stack` can also be installed via [ghcup], and on Linux it is usually available in your package manager.

[official stack documentation]: https://docs.haskellstack.org/en/stable/README/#how-to-install

If you have `git`, you can now clone the repository and build:

``` bash
git clone https://github.com/lierdakil/pandoc-crossref.git
cd pandoc-crossref
git checkout <commit/tag/branch>
stack install
```

If you don't have `git`, just download the sources for your preferred commit/branch/tag via the GitHub interface, and run `stack install` in the directory that contains `stack.yaml` file.

This will install pandoc-crossef executable to `$HOME/.local/bin`. You might also want to separately run `stack install pandoc-cli` in the same directory (i.e. the root of the repository, the one containing `stack.yaml` file)

### Installing as a nix flake

TL;DR:

```
nix profile install github:lierdakil/pandoc-crossref
```

will install the latest commit from the `master` branch. You can also specify a commit, branch or tag, e.g.:

```
nix profile install github:lierdakil/pandoc-crossref/71c8c8508c222bf4110794457fdf0391b05fb9a9
```

You can also get the corresponding `pandoc` version installed via

```
nix profile install github:lierdakil/pandoc-crossref#pandoc
```

Since you will generally want both, there's an option to install both at the
same time, too:

```
nix profile install github:lierdakil/pandoc-crossref#pandoc-with-crossref
```

Aside from added convenience, this guarantees pandoc and pandoc-crossref
versions to be consistent across updates.

Finally, you can start a nix shell with both `pandoc` and `pandoc-crossref`
using

```
nix develop github:lierdakil/pandoc-crossref
```

**Warning**: this uses [haskell.nix][] infrastructure for builds (because
Haskell support in Nix is borked, and has been for a long time). This means that
unless you use their substituters, you'll build multiple GHC versions from
source. To avoid that, add `https://cache.iog.io` to `substituters` in
`nix.conf` and `hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=` to
`trusted-public-keys`.

[haskell.nix]: https://github.com/input-output-hk/haskell.nix

You can also use pandoc-crossref's binary cache by adding `https://pandoc-crossref.cachix.org` and `pandoc-crossref.cachix.org-1:LI9ABFTkGpPCTkUTzoopVSSpb1a26RSTJNMsqVbDtPM=` to `substituters` and `trusted-public-keys` respectively.

The flake includes both by default, so if you're a nix trusted user and accept
these configurations during flake evaluation those will be used automatically.

### Notice Fedora users

`cabal-install` package is not enough to build pandoc-crossref (see
[\#132](https://github.com/lierdakil/pandoc-crossref/issues/132)).
To get a sane Haskell build environment, you need to install the
`haskell-platform` package (`dnf install haskell-platform`).

While on topic, if you don't want to rebuild pandoc itself from source,
make sure you have `pandoc` and `ghc-pandoc-devel` dnf packages before
attempting to build pandoc-crossref.

## Usage

Usage information is available at
<https://lierdakil.github.io/pandoc-crossref/>

## Projects
The following projects use this filter:

- [TechnicalMarkdown](https://github.com/gabyx/TechnicalMarkdown)

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
-   Rui Chen
-   gesh
-   Raphael Das Gupta
-   Masamichi Hosoda
-   Felix Yan
-   Wlad
-   Wandmalfarbe
-   Vekhir
-   Silas Benson
-   shutingrz
-   Shaun Jackman
-   scoavoux
-   Salim B
-   Matthew Salganik
-   Jan-T. Brinkmann
-   hseg
-   Han Joosten
-   Hadrien Mary
-   Gleb Popov
-   Gabriel Nützi
-   David Arnold
-   Chris Black
-   Bart Mesuere
-   Albert

<!-- END CONTRIBUTORS LIST -->

This repository includes code from <https://github.com/roelvandijk/roman-numerals>, covered by a different license. See
[licenses/LICENSE.roman-numerals](https://github.com/lierdakil/pandoc-crossref/blob/master/licenses/LICENSE.roman-numerals)
for details.

# How to bump pandoc version

1. Change `PANDOC_VERSION` in `.github/workflows/haskell.yml` to the new Pandoc version.
2. Relax version bounds in `package.yaml` if needed.
3. Run `make update`. You need at least `nix`, `stack` and `cabal` (i.e. cabal-install) installed and in `PATH`.

    If it doesn't do anything, consider nuking `cabal.project.freeze`, `flake.lock`, `stack.yaml` and `stack.yaml.lock` and trying again.
4. Build and test.
5. Fix broken tests.

    Note that you can regenerate most golden tests with either
    `make regen-test-fixtures` if using Nix, or just running `./mkcheck.sh` and
    `./mkinttest.sh` with appropriate `pandoc` and `pandoc-crossref` binaries in
    scope (so e.g. via `stack exec`).
6. Repeat 4-5 until all tests pass.
