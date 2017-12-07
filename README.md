# pandoc-crossref filter [![Build Status](https://travis-ci.org/lierdakil/pandoc-crossref.svg?branch=master)](https://travis-ci.org/lierdakil/pandoc-crossref) [![Build status](https://ci.appveyor.com/api/projects/status/v04mfbglpcdqfln4/branch/master?svg=true)](https://ci.appveyor.com/project/lierdakil/pandoc-crossref/branch/master)

pandoc-crossref is a pandoc filter for numbering figures, equations, tables and cross-references to them.

Input file (like [demo.md][demo-md]) can be converted into [html][html], [latex][latex], [pdf][pdf], [md][md] or other formats.

Optionally, you can use cleveref for latex/pdf output, e.g. [cleveref pdf][cpdf], [cleveref latex][clatex], and listings package, e.g. [listings pdf][lpdf], [listings latex][llatex]

You can also enable per-chapter numbering (as with `--chapters` for latex output). You need to specify `-M chapters` for non-latex/pdf output however. Examples: [html][chap-html], [markdown][chap-markdown], [latex][chap-latex], [pdf][chap-pdf].

[demo-md]: http://lierdakil.github.io/pandoc-crossref/demo/demo.md
[html]: http://lierdakil.github.io/pandoc-crossref/demo/output.html
[latex]: http://lierdakil.github.io/pandoc-crossref/demo/output.latex
[pdf]: http://lierdakil.github.io/pandoc-crossref/demo/output.pdf
[md]: http://lierdakil.github.io/pandoc-crossref/demo/output.md
[chap-html]: http://lierdakil.github.io/pandoc-crossref/demo/output-chapters.html
[chap-latex]: http://lierdakil.github.io/pandoc-crossref/demo/output-chapters.latex
[chap-markdown]: http://lierdakil.github.io/pandoc-crossref/demo/output-chapters.md
[chap-pdf]: http://lierdakil.github.io/pandoc-crossref/demo/output-chapters.pdf
[clatex]: http://lierdakil.github.io/pandoc-crossref/demo/output-cref.latex
[cpdf]: http://lierdakil.github.io/pandoc-crossref/demo/output-cref.pdf
[llatex]: http://lierdakil.github.io/pandoc-crossref/demo/output-listings.latex
[lpdf]: http://lierdakil.github.io/pandoc-crossref/demo/output-listings.pdf


This work is inspired by [pandoc-fignos][1] and [pandoc-eqnos][2] by @tomduck.

[1]: https://github.com/tomduck/pandoc-fignos
[2]: https://github.com/tomduck/pandoc-eqnos

This package tries to use latex labels and references if output type is LaTeX. It also tries to supplement rudimentary LaTeX configuration that should mimic metadata configuration by setting `header-includes` variable.

## Installation

Assuming you already installed [Haskell platform](http://hackage.haskell.org/platform/), you can install pandoc-crossref with cabal:

```bash
cabal update
cabal install pandoc-crossref
```

However, I highly recommend you use a sandbox for installation, e.g.

```bash
cabal update
mkdir pandoc-crossref
cd pandoc-crossref
cabal sandbox init
cabal install pandoc-crossref
```

This will get `pandoc-crossref` installed into `.cabal-sandbox/bin`. Pandoc will also be built, if it's not installed as a Haskell library system-wide. You might also want to install `pandoc-citeproc` in the same sandbox, if that's the case (`cabal install pandoc-citeproc`).

There are a few pre-built executables available at [releases page](https://github.com/lierdakil/pandoc-crossref/releases/latest) for Windows, OSX and Linux. Bear in mind that those are a product of an automated build script, and as such, provided as-is, with zero guarantees.

### Notice Fedora users

`cabal-install` package is not enough to build pandoc-crossref (see #132). To get sane Haskell build environment, you need to install `haskell-platform` package (`yum install haskell-platform`).

While on topic, if you don't want to rebuild Pandoc itself from source, make sure you have `pandoc` and `ghc-pandoc-devel` yum packages before attempting to build pandoc-crossref.

## Usage

Usage information is available at <https://lierdakil.github.com/pandoc-crossref/>

# License

This software is licensed under GNU GPL 2. See [LICENSE](https://github.com/lierdakil/pandoc-crossref/blob/master/LICENSE) for details.

© 2016 Nikolay Yakimov et al

Contributors (per GPL, holders of copyright on their respective contributions):

* Nikolay Yakimov
* Raphael Das Gupta
* Felix Yan
* Wlad
* scoavoux
* Matthew Salganik
* Masamichi Hosoda
* Hadrien Mary
* Chris Black
* Bart Mesuere
