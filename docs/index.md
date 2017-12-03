---
author: Nikolay Yakimov
date: November 2017
title: 'pandoc-crossref(1)'
---

pandoc-crossref is a pandoc filter for numbering figures, equations, tables and cross-references to them.

# Caveats

## LaTeX output and `--include-in-header`

pandoc-crossref uses metadata variable `header-includes` to add LaTeX definitions to output. However, Pandoc's command line option `--include-in-header`/`-H` overrides this variable. If you need to use `--include-in-header`, add pandoc-crossref-specific definitions as well. See [LaTeX customization](#latex-customization) for more information.

## pandoc-citeproc and pandoc-crossref

Since pandoc-crossref uses the same citation syntax as pandoc-citeproc, you *have* to run former *before* latter. For example:

```
pandoc -F pandoc-crossref -F pandoc-citeproc file.md -o file.html
```

## Note on leading/trailing spaces in metadata options

Leading and trailing spaces in YAML metadata will most likely be stripped by either YAML parser or Pandoc itself. If you need leading and/or trailing spaces in pandoc-crossref metadata variables, use html entity for space instead, i.e. `&#32;`. For example, if you want reference ranges to be delimited by a dash with spaces (e.g. `2 - 5`), include the following in YAML metadata:

```yaml
rangeDelim: '&#32;-&#32;'
```

or pass `-MrangeDelim='&#32;-&#32;'` to pandoc on command line.

You can use other html entites of course, like `&nbsp;` etc.

# Syntax

Syntax is loosely based on discussion in <https://github.com/jgm/pandoc/issues/813>

## Image labels

```markdown
![Caption](file.ext){#fig:label}
```

To label an (implicit) figure, append `{#fig:label}` (with `label` being something unique to reference this figure by) immediately after image definition.

This only works on implicit figures, i.e. an image occurring by itself in a paragraph (which will be rendered as a figure with caption by pandoc)

Image block and label *can not* be separated by spaces.

### Subfigures

It's possible to group figures as subfigures. Basic syntax is as follows:

```markdown
<div id="fig:figureRef">
![subfigure 1 caption](image1.png){#fig:figureRefA}

![subfigure 2 caption](image2.png){#fig:figureRefB}

Caption of figure
</div>
```

To sum up, subfigures are made with a div having a figure `id`. Contents of said div consist of several paragraphs. All but last paragraphs contain one subfigure each, with captions, images and (optionally) reference attributes. Last paragraph contains figure caption.

If you put more than one figure in the paragraph, those will still be rendered, but Pandoc will omit subfigure caption in most outputs (but it will work as expected with LaTeX). You can use output-specific hacks to work around that, or use `subfigGrid` (see below).

Output is customizable, with metadata fields. See [Customization](#customization) for more information.

Default settings will produce the following equivalent Markdown from example above:

```markdown
<div id="fig:figureRef" class="subfigures">

![a](image1.png){#fig:figureRefA}

![b](image2.png){#fig:figureRefB}

Figure 1: Caption of figure. a — subfigure 1 caption, b — subfigure 2
caption

</div>
```

References to subfigures will be rendered as `figureNumber (subfigureNumber)`, e.g., in this particular example, `[@fig:figureRefA]` will produce `fig. 1 (a)`.

You can add `nocaption` class to an image to suppress subfigure caption altogether. Note that it will still be counted.

#### Subfigure grid

If you need to align subfigures in a grid, and using output format styles is not an option, you can use `subfigGrid` option. That will typeset subfigures inside a table.

Rows are formed by different paragraphs, with each image in a separate column.

Column widths will be taken from `width` attributes of corresponding images, e.g.

```markdown
<div id="fig:coolFig">
![caption a](coolfiga.png){#fig:cfa width=30%}
![caption b](coolfigb.png){#fig:cfb width=60%}
![caption c](coolfigb.png){#fig:cfc width=10%}

![caption d](coolfigd.png){#fig:cfd}
![caption e](coolfige.png){#fig:cfe}
![caption f](coolfigf.png){#fig:cff}

Cool figure!
</div>
```

will produce a table with columns of 30%, 60% and 10% respectively.

Only first row of images is considered for table width computation, other rows are completely ignored.

*Anything* except images is silently ignored. So any text, spaces, soft line breaks etc will silently disappear from output. That doesn't apply to caption paragraph, obviously.

All images will have width attribute automatically set to `100%` in order to fill whole column.

Specifying width in anything but `%` will throw an error.

If width for some images in first row is not specified, those will span equally in the remaining space.

If width isn't specified for any image in first row, those will span equally on 99% of page width (due to Pandoc otherwise omitting width attribute for table).

This option is ignored with LaTeX output, but paragraph breaks should produce similar effect, so images should be typeset correctly. TL;DR you don't need `subfigGrid` enabled for it to work with LaTeX, but you can still enable it.

## Equation labels

```markdown
$$ math $$ {#eq:label}
```

To label a display equation, append `{#eq:label}` (with `label` being something unique to reference this equation by) immediately after math block.

Math block and label *can* be separated by one or more spaces.

You can also number all display equations with `autoEqnLabels` metadata setting (see below). Note, however, that you won't be able to reference equations without explicit labels.

Equations numbers will be typeset inside math with `\qquad` before them. If you want to use tables instead, use `tableEqns` option.  Depending on output format, tables might work better or worse than `\qquad`.

## Table labels

```markdown
a   b   c
--- --- ---
1   2   3
4   5   6

: Caption {#tbl:label}
```

To label a table, append `{#tbl:label}` at the end of table caption (with `label` being something unique to reference this table by). Caption and label *must* be separated by at least one space.

## Section labels

You can also reference sections of any level. Section labels use native pandoc syntax, but must start with "sec:", e.g.

```markdown
 Section {#sec:section}
```

You can also use `autoSectionLabels` variable to automatically prepend all section labels (automatically generated with pandoc included) with "sec:". Bear in mind that references can't contain periods, commas etc, so some auto-generated labels will still be unusable.

WARNING: With LaTeX output, you have to invoke pandoc with `--number-sections`, otherwise section labels won't work. It's also advised with other output formats, since with no numbers in section titles, it would be hard to navigate anyway.

### Section numbering

Pandoc doesn't properly support numbering sections in some output formats, and section reference labels (see below).

You can let pandoc-crossref handle section numbering instedad. This is done via `numberSections` and `sectionsDepth` metadata options.

`numberSections` controls if pandoc-crossref handles numbering sections, while `sectionsDepth` controls what sections are numbered.

Set `sectionsDepth` to `0` to make section numbering consistent with `chaptersDepth`.

If `sectionsDepth` value is lesser than `0`, all sections will be numbered.

Otherwise, only header levels up to and including `sectionsDepth` will be numbered.

## Section reference labels

***Not currently supported with LaTeX output***

If you want to reference some section by a pre-defined label instead of by number, you can specify section attribute `label`, like this:

```markdown
 Section {label="Custom Label"}
```

This label will be used instead of section number in `chapters` output and when referencing section directly (with `@sec:section`).

Note that with `chapters` output with depth>1, only given section will be referenced by custom label, e.g. with

```markdown
 Chapter 1.

# Section with custom label {#sec:scl label="SCL"}

![](figure.png){#fig:figure}
```

`@sec:scl` will translate into `sec. 1.SCL`, and `@fig:figure` into `fig. 1.SCL.1`

## Code Block labels

There are a couple options to add code block labels. Those work only if code block id starts with `lst:`, e.g. `{#lst:label}`

### `caption` attribute

`caption` attribute will be treated as code block caption. If code block has both id and `caption` attributes, it will be treated as numbered code block.

<pre>
```{#lst:code .haskell caption="Listing caption"}
main :: IO ()
main = putStrLn "Hello World!"
```
</pre>

### Table-style captions

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

It also allows to specify label in caption, as do tables, for example:

<pre>
```haskell
main :: IO ()
main = putStrLn "Hello World!"
```

: Listing caption {#lst:code}
</pre>

### Wrapping div

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

## References

```markdown
[@fig:label1;@fig:label2;...] or [@eq:label1;@eq:label2;...] or [@tbl:label1;@tbl:label2;...] or @fig:label or @eq:label or @tbl:label
```

Reference syntax heavily relies on citation syntax. Basic reference is created by writing `@`, then basically desired label with prefix. It is also possible to reference a group of objects, by putting them into brackets with `;` as separator. Similar objects will be grouped in order of them appearing in citation brackets, and sequential reference numbers will be shortened, e.g. `1,2,3` will be shortened to `1-3`.

You can capitalize first reference character to get capitalized prefix, e.g. `[@Fig:label1]` will produce `Fig. ...` by default. Capitalized prefixes are derived automatically by capitalizing first letter of every word in non-capitalized prefix, unless overridden with metadata settings. See [Customization](#customization) for more information.

### Linking references

To make references into hyperlinks to referenced element, enable `linkReferences` metadata option. This has no effect on LaTeX output, since in this case, hyperlinking references is handled with `hyperref` LaTeX package.

### Custom prefix per-reference

It's possible to provide your own prefix per-reference, f.ex. `[Prefix @reference]` will replace default prefix (`fig.`/`sec.`/etc) with prefix verbatim, e.g. `[Prefix @fig:1]` will be rendered as `Prefix 1` instead of `fig. 1`.

In citation group, citations with the same prefix will be grouped. So, for example `[A @fig:1; A @fig:2; B @fig:3]` will turn into `A 1, 2, B 3`. It can be used to an advantage, although it's a bit more cumbersome than it should be, e.g. `[Appendices @sec:A1; Appendices @sec:A2; Appendices @sec:A3]` will turn into `Appendices @A1-@A3` (with `@A1` and `@A3` being relevant section numbers). Note that non-contiguous sequences of identical prefixes *will not* be grouped.

***Not supported with cleveref LaTeX output.***

### Prefix suppression

Prepending `-` before `@`, like so `[-@citation]`, will suppress default prefix, e.g. `[-@fig:1]` will produce just `1` (or whatever number it happens to be) without `fig.` prefix.

In citation group, citations with and without prefixes will be in different groups. So `[-@fig:1; @fig:2; -@fig:3]` will be rendered as `1, fig. 2, 3`, so be careful with this feature. Again, non-contiguous sequences are not grouped together.

## Lists

It's possible to use raw latex commands `\listoffigures`, `\listoftables` and `listoflistings`, which will produce ordered list of figure/table/listings titles, in order of appearance in document.

`\listoflistings` depends on other options, and is defined in preamble, so it will work reliably only with standalone/pdf output.

# Usage

Run pandoc with `--filter` option, passing path to pandoc-crossref executable, or simply `pandoc-crossref`, if it's in PATH:

`pandoc --filter pandoc-crossref`

If you installed with cabal, it's most likely located in `$HOME/.cabal/bin` on \*NIX systems, `$HOME/Library/Haskell/bin` on Macs, or in `%AppData%\cabal\bin` on Windows.

## Customization

There are several parameters that can be set via YAML metadata (either by passing `-M` to `pandoc`, or by setting it in source markdown)

A list of variables follows.

### General options

* `cref`: if True, latex export will use `\cref` from cleveref package. Only relevant for LaTeX output. `\usepackage{cleveref}` will be automatically added to `header-includes`.
* `chapters`: if True, number elements as `chapter.item`, and restart `item` on each first-level heading (as `--chapters` for latex/pdf output)
* `chaptersDepth`, default `1`: header level to treat as "chapter". If `chaptersDepth>1`, then items will be prefixed with several numbers, corresponding to header numbers, e.g. `fig. 1.4.3`.
* `numberSections`, default `false`: if True, pandoc-crossref will prepend section number to section titles (as counted by pandoc-crossref itself).
* `sectionsDepth`, default `0`:
    - sectionsDepth < 0 -- number all sections
    - sectionsDepth == 0 -- be consistent with `chaptersDepths`
    - sectionsDepth > 0 -- number section levels up to and including `sectionsDepth`
* `listings`: if True, generate code blocks for `listings` package. Only relevant for LaTeX output. `\usepackage{listings}` will be automatically added to `header-includes`. You need to specify `--listings` option as well.
* `codeBlockCaptions`: if True, parse table-style code block captions.
* `autoSectionLabels`, default `false`: Automatically prefix all section labels with `sec:`. Note that this messes with pandoc's automatic header references.
* `autoEqnLabels`, default `false`: Automatically number all display equations (i.e. ones defined using `$$...$$`/`\[...\]`). Note that you won't be able to reference equations without explicit labels.
* `tableEqns`, default `false`: Typeset equations and equation numbers in tables instead of embedding numbers into equations themselves. Depending on output format, this might work better or worse.

### Item title format

* `figureTitle`, default `Figure`: Word(s) to prepend to figure titles, e.g. `Figure 1: Description`
* `tableTitle`, default `Table`: Word(s) to prepend to table titles, e.g. `Table 1: Description`
* `listingTitle`, default `Listing`: Word(s) to prepend to listing titles, e.g. `Listing 1: Description`
* `titleDelim`, default `:`: What to put between object number and caption text.

#### Subfigure-specific

See [Subfigures](#subfigures)

* `ccsDelim`, default `,&nbsp;`: delimiter for collected subfigure captions. See [Subfigures](#subfigures) and [Templates](#templates)
* `ccsLabelSep`, default `&nbsp;—&nbsp;`: delimiter used between subfigure label and subfigure caption in collected captions. See [Subfigures](#subfigures) and [Templates](#templates)
* `subfigGrid`, default `false`. If true, typeset subfigures inside a table. Ignored with LaTeX output. See [Subfigures](#subfigures)

### List titles

* `lofTitle`, default `# List of Figures`: Title for list of figures (lof)
* `lotTitle`, default `# List of Tables`: Title for list of tables (lot)
* `lolTitle`, default `# List of Listings`: Title for list of listings (lol)

### Reference format

* `figPrefix`, default `fig.`, `figs.`: Prefix for references to figures, e.g. `figs. 1-3`
* `eqnPrefix`, default `eq.`, `eqns.`: Prefix for references to equations, e.g. `eqns. 3,4`
* `tblPrefix`, default `tbl.`, `tbls.`: Prefix for references to tables, e.g. `tbl. 2`
* `lstPrefix`, default `lst.`, `lsts.`: Prefix for references to lists, e.g. `lsts. 2,5`
* `secPrefix`, default `sec.`, `secs.`: Prefix for references to sections, e.g. `secs. 2,5`
* `chapDelim`, default `.`: Delimiter between chapter number and item number.
* `rangeDelim`, default `-`: Delimiter between reference ranges, e.g. `eq. 2-5`
* `pairDelim`, default `, `: Delimiter between pair of reference ranges, e.g. `eq. 2-5 <and> 7-9`, or `eq. 2 <and> 7`, but `eq. 2, 4, 6`
* `lastDelim`, default `, `: Delimiter between second-to-last and last reference ranges, e.g. `eq. 2-5, 6-8 <and> 10`. `pairDelim` overrides this for cases of exactly two references/ranges.
* `refDelim`, default `, `: Delimiter between references, e.g. `eq. 2, 5, 7` or `eq. 2-4, 6-8`
* `linkReferences`, default `false`: Make references hyperlinks to the referenced element
* `nameInLink`, default `false`: For single-element references, inlcude prefix into hyperlink (when using `linkReferences`)

Note that none of the `*Delim` options are honored with cleveref output. Use cleveref's customization options instead.

`figPrefix`, `eqnPrefix`, `tblPrefix`, `lstPrefix` can be YAML arrays. That way, value at index corresponds to total number of references in group, f.ex.

```yaml
figPrefix:
  - "fig."
  - "figs."
```

Will result in all single-value references prefixed with "fig.", and all reference groups of two and more will be prefixed with "figs.":

```markdown
[@fig:one] -> fig. 1
[@fig:one; @fig:two] -> figs. 1, 2
[@fig:one; @fig:two; @fig:three] -> figs. 1-3
```

They can be YAML strings as well. In that case, prefix would be the same regardless of number of references.

They can also be used with first character capitalized, i.e. `FigPrefix`, etc. In this case, these settings will override default reference capitailzation settings.

### Custom numbering

See [Custom Numbering Schemes](#custom-numbering-schemes)

* `figLabels`, default unset: custom numbering scheme for figures.
* `subfigLabels`, default `alpha a`: custom numbering scheme for subfigures.
* `eqnLabels`, default unset: custom numbering scheme for equations.
* `tblLabels`, default unset: custom numbering scheme for tables.
* `lstLabels`, default unset: custom numbering scheme for listings.
* `secLabels`, default unset: custom numbering scheme for sections.

### Item title templates

See [Templates](#templates)

* `figureTemplate`, default `$$figureTitle$$ $$i$$$$titleDelim$$ $$t$$`: template for figure captions
* `tableTemplate`, default `$$tableTitle$$ $$i$$$$titleDelim$$ $$t$$`: template for table captions
* `listingTemplate`, default `$$listingTitle$$ $$i$$$$titleDelim$$ $$t$$`: template for listing captions

#### Subfigure templates

See [Subfigures](#subfigures)

* `subfigureTemplate`, default `$$figureTitle$$ $$i$$$$titleDelim$$ $$t$$. $$ccs$$`: template for subfigure divs captions.
* `subfigureChildTemplate`, default `$$i$$`: template for actual subfigure captions.
* `ccsTemplate`, default `$$i$$$$ccsLabelSep$$$$t$$`: template for collected subfigure captions.

### Reference templates

See [Templates](#templates)

* `figPrefixTemplate`, default `$$p$$&nbsp;$$i$$` -- figure reference template
* `eqnPrefixTemplate`, default `$$p$$&nbsp;$$i$$` -- equation reference template
* `tblPrefixTemplate`, default `$$p$$&nbsp;$$i$$` -- table reference template
* `lstPrefixTemplate`, default `$$p$$&nbsp;$$i$$` -- listing reference template
* `secPrefixTemplate`, default `$$p$$&nbsp;$$i$$` -- section reference template
* `refIndexTemplate`, default `$$i$$$$suf$$` -- individual reference index template
* `subfigureRefIndexTemplate`, default `$$i$$$$suf$$ ($$s$$)` -- subfigure reference index template

### LaTeX customization

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

## Templates

pandoc-crossref supports advanced caption customization via caption templates. Templates are specified as YAML metadata variables (see [Customization](#customization)), and are parsed as default Pandoc Markdown. Variables are specified with display math syntax, i.e. `$$var$$` in a template will be replaced with value of variable `var`.
Variables can be specified in YAML metadata block, or from command line (with `-M` switch). There are two special variables, that are set internally:

* `i` -- object number, possibly with chapter number (if `chapter=True`)
* `t` -- object caption, as given in source Markdown

Also there is a number of specific variables that are meaningful only in certain contexts:

* `ccs` -- collected subfigure captions. Only applicable to `subfigureTemplate`. Collected captions will be separated by `ccsDelim` and individual captions will be printed with `ccsTemplate`. See [Subfigures](#subfigures)
* `suf` -- reference suffix, applicable to `refIndexTemplate`, `subfigureRefIndexTemplate`
* `s` -- subfigure index, applicable to `subfigureRefIndexTemplate`

`xPrefixTemplate`, where `x` is `fig`, `eqn`, etc, are a special case. Those don't have `t` variable, since there is no caption in source markdown, but instead have `p` variable, that binds to relevant `xPrefix`. This is done this way, since actual prefix vaule can depend on `i`.

Please note that at the moment, templates are not supported with LaTeX/PDF output.

## Custom Numbering Schemes

It's possible to use other numbering schemes apart from arabic. This is controlled by several metadata options, consult [Customization](#customization) for a list. Possible values are:

- `arabic` -- arabic numbers (1, 2, 3 ...)
- `roman` -- roman numbers (I, II, III, IV, ...)
- `alpha x`, where `x` is first letter to start from. This will work for any letter, but will use UTF-8 codepage to determine what's next, so using something strange is not advised. For example, you can safely use `alpha a` to get lowercase latin letters for 26 figures. After that, it will get weird (since basic latin alphabet has 26 letters). Specifically, it will go into characters space (`{`, `|`, `}`, etc). You can consult http://www.fileformat.info/info/unicode/block/basic_latin/utf8test.htm for general idea on letter progression.
- list of strings. You can define a YAML array for numbers. Mapping is 1:1. For example,
    ```yaml
    figLabels: [α, β, γ, 1, 2, 3, I, II, III]
    ```
    will give first figure label 'α', second -- 'β', etc.

    Note that it will repeat last item in list indefinitely if there are more images than items in list.

## Settings file

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

pandoc-crossref will send this data to pandoc wrapped in lines of `---`. The YAML file's first line should specify a variable; it will not pass the variables if it is `---` or a blank line.

One could use this with pandoc-crossref as follows:

`pandoc -F pandoc-crossref.hs -M "crossrefYaml=$HOME/misc/pandoc-crossref-es.yaml"`

You can also use global configuration files, which are expected in `$HOME/.pandoc-crossref/config.yaml` and `$HOME/.pandoc-crossref/config-$FORMAT.yaml`, where `$FORMAT` is output format, f.ex. `latex` or `epub`. On Windows, `$HOME` in general resolves to user's root directory, e.g. `C:\Users\username\`.

Priorities are as follows (from highest to lowest):

- document metadata
- `crossrefYaml`/`$CWD/pandoc-crossref.yaml`
- `$HOME/.pandoc-crossref/config-$FORMAT.yaml`
- `$HOME/.pandoc-crossref/config.yaml`

# License

This software is licensed under GNU GPL v2
