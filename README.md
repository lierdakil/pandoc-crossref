# pandoc-crossref filter ![](https://travis-ci.org/lierdakil/pandoc-crossref.svg?branch=master)

pandoc-crossref is a pandoc filter for numbering figures, equations, tables and cross-references to them.

Input file (like [demo.md][demo-md]) can be converted into [html][html], [latex][latex], [pdf][pdf], [md][md] or other formats.

Optionally, you can use cleveref for latex/pdf output, e.g. [cleveref pdf][cpdf], [cleveref latex][clatex]

You can also enable per-chapter numbering (as with `--chapters` for latex output). You need to specify `-M chapters` for non-latex/pdf output however. Examples: [html][chap-html], [markdown][chap-markdown], [latex][chap-latex], [pdf][chap-pdf].

[demo-md]: https://raw.githubusercontent.com/lierdakil/pandoc-crossref/gh-pages/demo.md
[html]: http://lierdakil.github.io/pandoc-crossref/output.html
[latex]: http://lierdakil.github.io/pandoc-crossref/output.latex
[pdf]: http://lierdakil.github.io/pandoc-crossref/output.pdf
[md]: http://lierdakil.github.io/pandoc-crossref/output.md
[chap-html]: http://lierdakil.github.io/pandoc-crossref/output-chapters.html
[chap-latex]:http://lierdakil.github.io/pandoc-crossref/output-chapters.latex
[chap-markdown]:http://lierdakil.github.io/pandoc-crossref/output-chapters.md
[chap-pdf]:http://lierdakil.github.io/pandoc-crossref/output-chapters.pdf
[clatex]: http://lierdakil.github.io/pandoc-crossref/output-cref.latex
[cpdf]: http://lierdakil.github.io/pandoc-crossref/output-cref.pdf


Tested with pandoc 1.13.2.

This work is inspired by [pandoc-fignos][1] and [pandoc-eqnos][2] by @tomduck.

[1]: https://github.com/tomduck/pandoc-fignos
[2]: https://github.com/tomduck/pandoc-eqnos

This package tries to use latex labels and references if output type is latex.

## Syntax

Syntax is loosely based on discussion in <https://github.com/jgm/pandoc/issues/813>

### Image labels

```markdown
![Caption](file.ext){#fig:label}
```

To label an (implicit) figure, append `{#fig:label}` (with `label` being something unique to reference this figure by) immediately after image definition.

This only works on implicit figures, i.e. an image occurring by itself in a paragraph (which will be rendered as a figure with caption by pandoc)

Image block and label *can* be separated by one or more spaces.

### Equation labels

```markdown
$$ math $$ {#eq:label}
```

To label a display equation, append `{#eq:label}` (with `label` being something unique to reference this equation by) immediately after math block.

This only works if display math and label specification are in a paragraph of its own.

Math block and label *can* be separated by one or more spaces.

### Table labels

```markdown
a   b   c
--- --- ---
1   2   3
4   5   6

: Caption {#tbl:label}
```

To label a table, append `{#tbl:label}` at the end of table caption (with `label` being something unique to reference this table by). Caption and label *must* be separated by at least one space.

### References

```markdown
[@fig:label1;@fig:label2;...] or [@eq:label1;@eq:label2;...] or [@tbl:label1;@tbl:label2;...] or @fig:label or @eq:label or @tbl:label
```

Reference syntax heavily relies on citation syntax. Basic reference is created by writing `@`, then basically desired label with prefix. It is also possible to reference a group of objects *of the same type*, by putting them into brackets with `;` as separator. Sequential reference numbers will be shortened, e.g. `1,2,3` will be shortened to `1-3`.

### Lists

It's possible to use raw latex commands `\listoffigures` and `\listoftables`, which will produce ordered list of figure/table titles, in order of appearance in document.

## Installation

Assuming you already installed [Haskell platform](http://hackage.haskell.org/platform/), you can install pandoc-crossref with cabal:

```bash
cabal update
cabal install pandoc-crossref
```

## Usage

Run pandoc with `--filter` option, passing path to pandoc-crossref executable, or simply `pandoc-crossref`, if it's in PATH:

`pandoc --filter pandoc-crossref`

If you installed with cabal, it's most likely located in `$HOME/.cabal/bin` on \*NIX systems, or in `%AppData%\cabal\bin` on Windows.

### Customization

There are several parameters that can be set via YAML metadata (either by passing `-M` to `pandoc`, or by setting it in source markdown)

Following variables are supported:

* `cref`: if True, latex export will use `\cref` from cleveref package. It is user's responsibility to include relevant `\usepackage` directives in template
* `chapter`: if True, number elements as `chapter.item`, and restart `item` on each first-level heading (as `--chapters` for latex/pdf output)
* `figureTitle`, default `Figure`: Word(s) to prepend to figure titles, e.g. `Figure 1: Description`
* `tableTitle`, default `Table`: Word(s) to prepend to table titles, e.g. `Table 1: Description`
* `titleDelimiter`, default `:`: What to put between object number and caption text.
* `figPrefix`, default `fig.`: Prefix for references to figures, e.g. `fig. 1-3`
* `eqnPrefix`, default `eq.`: Prefix for references to equations, e.g. `eq. 3,4`
* `tblPrefix`, default `tbl.`: Prefix for references to tables, e.g. `tbl. 2`
* `chapDelim`, default `.`: Delimiter between chapter number and item number.
* `rangeDelim`, default `-`: Delimiter between reference ranges, e.g. `eq. 2-5`
* `lofTitle`, default `# List of Figures`: Title for list of figures (lof)
* `lotTitle`, default `# List of Tables`: Title for list of tables (lot)
* `figureTemplate`, default `\\[figureTitle\\] \\[i\\]\\[titleDelim\\] \\[t\\]`: template for figure captions, see [Templates](#templates)
* `tableTemplate`, default `\\[tableTitle\\] \\[i\\]\\[titleDelim\\] \\[t\\]`: template for table captions, see [Templates](#templates)

### Templates

pandoc-crossref supports advanced caption customization via caption templates. Templates are specified as YAML metadata variables (see [Customization](#customization)), and are parsed as default Pandoc Markdown. Variables are specified with display math syntax, i.e. `$$var$$` in a template will be replaced with value of variable `var`.
Variables can be specified in YAML metadata block, or from command line (with `-M` switch). There are two special variables, that are set internally:

* `i` -- object number, possibly with chapter number (if `chapter=True`)
* `t` -- object caption, as given in source Markdown

### Settings file

It is also possible to set variables used by pandoc-crossref with a separate YAML file. If a given variable is not set in metadata, then pandoc-crossref will attempt to read it from file specified by `crossrefYaml` metadata variable, or, if not set, from `pandoc-crossref.yaml` from current working directory. This allows for reusable configurations. One possible application is ad-hoc internationalization.

For example, consider `$HOME/misc/pandoc-crossref-es.yaml`:

```yaml
figureTitle: "Figura"
tableTitle: "Tabla"
figPrefix: "fig."
eqnPrefix: "ec."
tblPrefix: "tbl."
loftitle: "# Lista de figuras"
lotTitle: "# Lista de tablas"
```

One could use this with pandoc-crossref as follows:

`pandoc -F pandoc-crossref.hs -M "crossrefYaml=$HOME/misc/pandoc-crossref-es.yaml"`
