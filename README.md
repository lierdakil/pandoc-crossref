# pandoc-crossref filter ![](https://travis-ci.org/lierdakil/pandoc-crossref.svg?branch=master)

pandoc-crossref is a pandoc filter for numbering figures, equations, tables and cross-references to them.

Input file (like [demo.md][demo-md]) can be converted into [html][html], [latex][latex], [pdf][pdf], [md][md] or other formats.

Optionally, you can use cleveref for latex/pdf output, e.g. [cleveref pdf][cpdf], [cleveref latex][clatex], and listings package, e.g. [listings pdf][lpdf], [listings latex][llatex]

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
[llatex]: http://lierdakil.github.io/pandoc-crossref/output-listings.latex
[lpdf]: http://lierdakil.github.io/pandoc-crossref/output-listings.pdf


Tested with pandoc 1.13.2 and 1.14.

This work is inspired by [pandoc-fignos][1] and [pandoc-eqnos][2] by @tomduck.

[1]: https://github.com/tomduck/pandoc-fignos
[2]: https://github.com/tomduck/pandoc-eqnos

This package tries to use latex labels and references if output type is LaTeX. It also tries to supplement rudimentary LaTeX configuration that should mimic metadata configuration by setting `header-includes` variable.

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

### Section labels

You can also reference sections of any level. Section labels use native pandoc syntax, but must start with "sec:", e.g.

```markdown
# Section {#sec:section}
```

You can also use `autoSectionLabels` variable to automatically prepend all section labels (automatically generated with pandoc included) with "sec:". Bear in mind that references can't contain periods, commas etc, so some auto-generated labels will still be unusable.

### Section reference labels

***Not currently supported with LaTeX output***

If you want to reference some section by a pre-defined label instead of by number, you can specify section attribute `label`, like this:

```markdown
# Section {label="Custom Label"}
```

This label will be used instead of section number in `chapters` output and when referencing section directly (with `@sec:section`).

Note that with `chapters` output with depth>1, only given section will be referenced by custom label, e.g. with

```markdown
# Chapter 1.

## Section with custom label {#sec:scl label="SCL"}

![](figure.png){#fig:figure}
```

`@sec:scl` will translate into `sec. 1.SCL`, and `@fig:figure` into `fig. 1.SCL.1`

### Code Block labels

There are a couple options to add code block labels. Those work only if code block id starts with `lst:`, e.g. `{#lst:label}`

#### `caption` attribute

`caption` attribute will be treated as code block caption. If code block has both id and `caption` attributes, it will be treated as numbered code block.

<pre>
```{#lst:code .haskell caption="Listing caption"}
main :: IO ()
main = putStrLn "Hello World!"
```
</pre>

#### Table-style captions

Enabled with `codeBlockCaptions` metadata option. If code block is immediately
adjacent to paragraph, starting with `Listing: ` or `: `, said paragraph will be
treated as code block caption.

<pre>
Listing: Listing caption

```{#lst:code .haskell}
main :: IO ()
main = putStrLn "Hello World!"
```
</pre>

or

<pre>
```{#lst:code .haskell}
main :: IO ()
main = putStrLn "Hello World!"
```

: Listing caption
</pre>

#### Wrapping div

Wrapping code block without label in a div with id `lst:...` and class, starting with `listing`, and adding paragraph before code block, but inside div, will treat said paragraph as code block caption.

<pre>
&lt;div id="lst:code" class="listing"&gt;
Listing caption
```{.haskell}
main :: IO ()
main = putStrLn "Hello World!"
```
&lt;/div&gt;
</pre>

### References

```markdown
[@fig:label1;@fig:label2;...] or [@eq:label1;@eq:label2;...] or [@tbl:label1;@tbl:label2;...] or @fig:label or @eq:label or @tbl:label
```

Reference syntax heavily relies on citation syntax. Basic reference is created by writing `@`, then basically desired label with prefix. It is also possible to reference a group of objects, by putting them into brackets with `;` as separator. Similar objects will be grouped in order of them appearing in citation brackets, and sequential reference numbers will be shortened, e.g. `1,2,3` will be shortened to `1-3`.

You can capitalize first reference character to get capitalized prefix, e.g. `[@Fig:label1]` will produce `Fig. ...` by default. Capitalized prefixes are derived automatically by capitalizing first letter of every word in non-capitalized prefix, unless overriden with metadata settings. See [Customization](#Customization) for more information.

### Lists

It's possible to use raw latex commands `\listoffigures`, `\listoftables` and `listoflistings`, which will produce ordered list of figure/table/listings titles, in order of appearance in document.

`\listoflistings` depends on other options, and is defined in preamble, so it will work reliably only with standalone/pdf output.

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

* `cref`: if True, latex export will use `\cref` from cleveref package. Only relevant for LaTeX output. `\usepackage{cleveref}` will be automatically added to `header-includes`.
* `chapters`: if True, number elements as `chapter.item`, and restart `item` on each first-level heading (as `--chapters` for latex/pdf output)
* `chaptersDepth`, default `1`: header level to treat as "chapter". If `chaptersDepth>1`, then items will be prefixed with several numbers, corresponding to header numbers, e.g. `fig. 1.4.3`.
* `listings`: if True, generate code blocks for `listings` package. Only relevant for LaTeX output. `\usepackage{listings}` will be automatically added to `header-includes`. You need to specify `--listings` option as well.
* `codeBlockCaptions`: if True, parse table-style code block captions.
* `figureTitle`, default `Figure`: Word(s) to prepend to figure titles, e.g. `Figure 1: Description`
* `tableTitle`, default `Table`: Word(s) to prepend to table titles, e.g. `Table 1: Description`
* `listingTitle`, default `Listing`: Word(s) to prepend to listing titles, e.g. `Listing 1: Description`
* `titleDelimiter`, default `:`: What to put between object number and caption text.
* `figPrefix`, default `fig.`, `figs.`: Prefix for references to figures, e.g. `figs. 1-3`
* `eqnPrefix`, default `eq.`, `eqns.`: Prefix for references to equations, e.g. `eqns. 3,4`
* `tblPrefix`, default `tbl.`, `tbls.`: Prefix for references to tables, e.g. `tbl. 2`
* `lstPrefix`, default `lst.`, `lsts.`: Prefix for references to lists, e.g. `lsts. 2,5`
* `secPrefix`, default `sec.`, `secs.`: Prefix for references to sections, e.g. `secs. 2,5`
* `autoSectionLabels`, default `false`: Automatically prefix all section labels with `sec:`. Note that this messes with pandoc's automatic header references.
* `chapDelim`, default `.`: Delimiter between chapter number and item number.
* `rangeDelim`, default `-`: Delimiter between reference ranges, e.g. `eq. 2-5`
* `lofTitle`, default `# List of Figures`: Title for list of figures (lof)
* `lotTitle`, default `# List of Tables`: Title for list of tables (lot)
* `lolTitle`, default `# List of Listings`: Title for list of listings (lol)
* `figureTemplate`, default `\\[figureTitle\\] \\[i\\]\\[titleDelim\\] \\[t\\]`: template for figure captions, see [Templates](#templates)
* `tableTemplate`, default `\\[tableTitle\\] \\[i\\]\\[titleDelim\\] \\[t\\]`: template for table captions, see [Templates](#templates)
* `listingTemplate`, default `\\[tableTitle\\] \\[i\\]\\[titleDelim\\] \\[t\\]`: template for listing captions, see [Templates](#templates)

`figPrefix`, `eqnPrefix`, `tblPrefix`, `lstPrefix` can be YAML arrays. That way, value at index corresponds to total number of references in group, f.ex.

```yaml
figPrefix:
  - "fig."
  - "figs."
```

Will result in all single-value references prefixed with "fig.", and all reference groups of two and more prefixed with "figs.":

```markdown
[@fig:one] -> fig. 1
[@fig:one; @fig:two] -> figs. 1, 2
[@fig:one; @fig:two; @fig:three] -> figs. 1-3
```

They can be YAML strings as well. In that case, prefix would be the same regardless of number of references.

They can also be used with first character capitalized, i.e. `FigPrefix`, etc. In this case, these settings will override default reference capitailzation settings.

#### LaTeX customization

Support for above variables with LaTeX/PDF output is limited. In particular, the following variables are honored:

* `figureTitle`
* `tableTitle`
* `listingTitle`
* `lofTitle` -- ignores formatting
* `lotTitle` -- ignores formatting
* `lolTitle` -- ignores formatting
* `*Prefix`, upper-/lowercase and single/plural form. Note that with cleveref output, if `*Prefix` is array, only first two items are used, and the rest is ignored.

Templates are *not* supported.

You can add arbitrary LaTeX commands to document header, however, using `header-includes` metadata field. Please bear in mind, that pandoc-crossref up to and including 0.1.2.1 requires `header-includes` to be YAML array, e.g.

```yaml
header-includes:
    - "\\newcommand{\\pcdoc}{Pandoc-crossref documentation}"
```

This will be added *before* any customization applied by pandoc-crossref. For a complete list of what is added to template, consult [ModifyMeta.hs][ModifyMeta.hs].

[ModifyMeta.hs]: https://github.com/lierdakil/pandoc-crossref/blob/master/src/Util/ModifyMeta.hs

### Templates

pandoc-crossref supports advanced caption customization via caption templates. Templates are specified as YAML metadata variables (see [Customization](#customization)), and are parsed as default Pandoc Markdown. Variables are specified with display math syntax, i.e. `$$var$$` in a template will be replaced with value of variable `var`.
Variables can be specified in YAML metadata block, or from command line (with `-M` switch). There are two special variables, that are set internally:

* `i` -- object number, possibly with chapter number (if `chapter=True`)
* `t` -- object caption, as given in source Markdown

Please note that at the moment, templates are not supported with LaTeX/PDF output.

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
