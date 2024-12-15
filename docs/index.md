---
author: Nikolay Yakimov
date: December 2024
title: 'pandoc-crossref(1)'
---

pandoc-crossref is a pandoc filter for numbering figures, equations,
tables and cross-references to them.

Bug tracker: <https://github.com/lierdakil/pandoc-crossref/issues>

* TOC
{:toc}

# Caveats

## LaTeX input (a.k.a. converting LaTeX to docx/epub/etc)

The principal aim of pandoc-crossref is to add references to Markdown.

Other input formats may happen to work also (basically if you can make Pandoc
citation syntax to work), but that's more of a happy coincidence than a
deliberate design decision.

In particular, LaTeX syntax is generally not recognized, although under some
specific conditions it may seem to kinda-sorta work.

Converting TeX to anything editable in general is nigh impossible, with TeX
being a Turing-complete layout language rather than a markup language. That was
one of the motivations for pandoc-crossref in the first place.

All that said, if you really need to use LaTeX as an input format, see the
discussions in <https://github.com/lierdakil/pandoc-crossref/issues/250>.

For best results, consider using a purpose-built filter instead of
pandoc-crossref. For example,
[pandoc-text-numbering](https://github.com/fncokg/pandoc-tex-numbering).

## LaTeX output and `--include-in-header`

pandoc-crossref uses metadata variable `header-includes` to add LaTeX
definitions to output. However, Pandoc's command line option
`--include-in-header`/`-H` overrides this variable. If you need to use
`--include-in-header`, add pandoc-crossref-specific definitions as well.
See [LaTeX customization](#latex-customization) for more information.

## Note on non-standard LaTeX templates

If you're using non-standard pandoc LaTeX template (this includes the cases where document class is not `article` or `book` close enough to that), pandoc-crossref might not be able to configure that properly. You might to configure LaTeX in the template manually.

## Note on LaTeX and `chapters` option

Because pandoc-crossref offloads all numbering to LaTeX if it can, `chapters: true` has no direct effect on LaTeX output. You have to specify Pandoc's `--top-level-division=chapter` option, which should hopefully configure LaTeX appropriately.

It's a good idea to specify `--top-level-division=chapter` for any output format actually, because pandoc-crossref can't signal pandoc you want to use chapters, and vice versa.

## citeproc and pandoc-crossref

Since pandoc-crossref uses the same citation syntax as citeproc,
you *have* to run former *before* latter. For example:

    pandoc -F pandoc-crossref --citeproc file.md -o file.html

## Note on leading/trailing spaces in metadata options

Leading and trailing spaces in YAML metadata will most likely be
stripped by either YAML parser or Pandoc itself. If you need leading
and/or trailing spaces in pandoc-crossref metadata variables, use html
entity for space instead, i.e. `&#32;`. For example, if you want
reference ranges to be delimited by a dash with spaces (e.g. `2 - 5`),
include the following in YAML metadata:

``` yaml
rangeDelim: '&#32;-&#32;'
```

or pass `-MrangeDelim='&#32;-&#32;'` to pandoc on command line.

You can use other html entites of course, like `&nbsp;` etc.

# Syntax

Syntax is loosely based on discussion in
<https://github.com/jgm/pandoc/issues/813>

## Image labels

``` markdown
![Caption](file.ext){#fig:label}
```

To label an (implicit) figure, append `{#fig:label}` (with `label` being
something unique to reference this figure by) immediately after image
definition.

This only works on implicit figures, i.e. an image occurring by itself
in a paragraph (which will be rendered as a figure with caption by
pandoc)

Image block and label *can not* be separated by spaces.

### Subfigures

It's possible to group figures as subfigures. Basic syntax is as
follows:

``` markdown
<div id="fig:figureRef">
![subfigure 1 caption](image1.png){#fig:figureRefA}

![subfigure 2 caption](image2.png){#fig:figureRefB}

Caption of figure
</div>
```

To sum up, subfigures are made with a div having a figure `id`. Contents
of said div consist of several paragraphs. All but last paragraphs
contain one subfigure each, with captions, images and (optionally)
reference attributes. Last paragraph contains figure caption.

If you put more than one figure in the paragraph, those will still be
rendered, but Pandoc will omit subfigure caption in most outputs (but it
will work as expected with LaTeX). You can use output-specific hacks to
work around that, or use `subfigGrid` (see below).

Output is customizable, with metadata fields. See
[Customization](#customization) for more information.

Default settings will produce the following equivalent Markdown from
example above:

``` markdown
<div id="fig:figureRef" class="subfigures">

![a](image1.png){#fig:figureRefA}

![b](image2.png){#fig:figureRefB}

Figure 1: Caption of figure. a — subfigure 1 caption, b — subfigure 2
caption

</div>
```

References to subfigures will be rendered as
`figureNumber (subfigureNumber)`, e.g., in this particular example,
`[@fig:figureRefA]` will produce `fig. 1 (a)`.

You can add `nocaption` class to an image to suppress subfigure caption
altogether. Note that it will still be counted.

#### Subfigure grid

If you need to align subfigures in a grid, and using output format
styles is not an option, you can use `subfigGrid` option. That will
typeset subfigures inside a table.

Rows are formed by different paragraphs, with each image in a separate
column.

Column widths will be taken from `width` attributes of corresponding
images, e.g.

``` markdown
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

Only first row of images is considered for table width computation,
other rows are completely ignored.

*Anything* except images is silently ignored. So any text, spaces, soft
line breaks etc will silently disappear from output. That doesn't apply
to caption paragraph, obviously.

All images will have width attribute automatically set to `100%` in
order to fill whole column.

Specifying width in anything but `%` will throw an error.

If width for some images in first row is not specified, those will span
equally in the remaining space.

If width isn't specified for any image in first row, those will span
equally on 99% of page width (due to Pandoc otherwise omitting width
attribute for table).

This option is ignored with LaTeX output, but paragraph breaks should
produce similar effect, so images should be typeset correctly. TL;DR you
don't need `subfigGrid` enabled for it to work with LaTeX, but you can
still enable it.

## Equation labels

``` markdown
$$ math $$ {#eq:label}
```

To label a display equation, append `{#eq:label}` (with `label` being
something unique to reference this equation by) immediately after math
block.

Math block and label *can* be separated by one or more spaces.

You can also number all display equations with `autoEqnLabels` metadata
setting (see below). Note, however, that you won't be able to reference
equations without explicit labels.

Equations numbers will be typeset inside math with `\qquad` before them.
If you want to use tables instead, use `tableEqns` option. Depending on
output format, tables might work better or worse than `\qquad`.

Alternatively, for formats that support it, you can use arbitrary LaTeX command accepting a single argument (that is, label text) for typesetting. A common example is `\tag`. Use `equationNumberTeX` metadata variable for that (set to  `\qquad` by default).

Beware that `eqnIndexTemplate` gets applied first, so you'll likely want to set it to plain index as well.

For instance, to use `\tag`, you would have the following in your metadata:

```yaml
equationNumberTeX: \\tag
eqnIndexTemplate: $$i$$
```

These options don't affect LaTeX output (which offloads numbering to the LaTeX engine).

For advanced usage, see `eqnInlineTemplate`, `eqnBlockTemplate`.

## Table labels

``` markdown
a   b   c
--- --- ---
1   2   3
4   5   6

: Caption {#tbl:label}
```

To label a table, append `{#tbl:label}` at the end of table caption
(with `label` being something unique to reference this table by).
Caption and label *must* be separated by at least one space.

## Section labels

You can also reference sections of any level. Section labels use native
pandoc syntax, but must start with "sec:", e.g.

``` markdown
 Section {#sec:section}
```

You can also use `autoSectionLabels` variable to automatically prepend
all section labels (automatically generated with pandoc included) with
"sec:". Bear in mind that references can't contain periods, commas etc,
so some auto-generated labels will still be unusable.

WARNING: With LaTeX output, you have to invoke pandoc with
`--number-sections`, otherwise section labels won't work. It's also
advised with other output formats, since with no numbers in section
titles, it would be hard to navigate anyway.

### Section numbering

Pandoc doesn't properly support numbering sections in some output
formats, and section reference labels (see below).

You can let pandoc-crossref handle section numbering instead. This is
done via `numberSections` and `sectionsDepth` metadata options.

`numberSections` controls if pandoc-crossref handles numbering sections,
while `sectionsDepth` controls what sections are numbered.

Additionally, with `numberSections`, if the first heading in your document is
level 2 or more, pandoc-crossref will assume you meant to have implicit
headings with previous levels, and will assign those phantom implicit headings
the index `1`. Without `numberSections`, the behaviour is consistent with
pandoc, that is, missing headings will be assigned the index `0`.

Set `sectionsDepth` to `0` to make section numbering consistent with
`chaptersDepth`.

If `sectionsDepth` value is lesser than `0`, all sections will be
numbered.

Otherwise, only header levels up to and including `sectionsDepth` will
be numbered.

You can also supply a custom section header template via `secHeaderTemplate`
metadata option. The following variables are supported:

-   `$$i$$` -- formatted section number, according to `sectionsDepth`
-   `$$t$$` -- original section header text
-   `$$n$$` -- 0-indexed section level (0 is the topmost)

See [section on templates](#templates) for more information

## Reference labels

***Not currently supported with LaTeX output***

If you want to reference some object by a pre-defined label instead of
by number, you can specify attribute `label`, like this:

```markdown
# Section {label="Custom Label"}

![Figure](fig.png){fig:fig1 label="Custom label"}
```

Note that to use this with equations and tables, you need to use fenced div/span
syntax, not the short syntax:

```markdown

:::{#tbl:table label="T"}
a   b   c
--- --- ---
1   2   3
4   5   6

: Caption
:::

[$$y = e^x$$]{#eq:equation label="E"}

```

This label will be used instead of a number in `chapters` output for sections
and when referencing the element directly.

Note that with `chapters` output with depth\>1, only the given section will be
referenced by the custom label, e.g. with

``` markdown
 Chapter 1.

# Section with custom label {#sec:scl label="SCL"}

![](figure.png){#fig:figure}
```

`@sec:scl` will translate into `sec. 1.SCL`, and `@fig:figure` into
`fig. 1.SCL.1`

## Manual numbering adjustment

***Not currently supported with LaTeX output***

For cases when you need to manually adjust numbering, you can specify the
`number` attribute on the object. It will set the internal object counter for
the annotated object to the number specified, and all the following objects of
this type will count from that. Conceptually, this is similar to document
processors' "start from..." etc.

Same as with `label` attributes, to use this with equations and tables, you need
to use fenced div/span syntax, not the short syntax.

## Code Block labels

There are a couple options to add code block labels. Those work only if
code block id starts with `lst:`, e.g. `{#lst:label}`

### `caption` attribute

`caption` attribute will be treated as code block caption. If code block
has both id and `caption` attributes, it will be treated as numbered
code block.

<pre>
```{#lst:code .haskell caption="Listing caption"}
main :: IO ()
main = putStrLn "Hello World!"
```
</pre>
### Table-style captions

Enabled with `codeBlockCaptions` metadata option. If code block is
immediately adjacent to paragraph, starting with `Listing:` or `:`, said
paragraph will be treated as code block caption.

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

Wrapping code block without label in a div with id `lst:...` and class,
starting with `listing`, and adding paragraph before code block, but
inside div, will treat said paragraph as code block caption.

<pre>
&lt;div id="lst:code" class="listing"&gt;
Listing caption
```{.haskell}
main :: IO ()
main = putStrLn "Hello World!"
```
&lt;/div&gt;
</pre>

Any additional attributes and classes on the wrapping div will be merged with classes/attributes on the listing itself. In case of duplicate attributes, the behaviour is unspecified, but likely either both values will end up in the output, or div attributes will take precedence. This is important to keep in mind if you need to do some additional post-processing.

## References

``` markdown
[@fig:label1;@fig:label2;...] or [@eq:label1;@eq:label2;...] or [@tbl:label1;@tbl:label2;...] or @fig:label or @eq:label or @tbl:label
```

Reference syntax heavily relies on citation syntax. Basic reference is
created by writing `@`, then basically desired label with prefix. It is
also possible to reference a group of objects, by putting them into
brackets with `;` as separator. Similar objects will be grouped in order
of them appearing in citation brackets, and sequential reference numbers
will be shortened, e.g. `1,2,3` will be shortened to `1-3`.

You can capitalize first reference character to get capitalized prefix,
e.g. `[@Fig:label1]` will produce `Fig. ...` by default. Capitalized
prefixes are derived automatically by capitalizing first letter of every
word in non-capitalized prefix, unless overridden with metadata
settings. See [Customization](#customization) for more information.

### Linking references

To make references into hyperlinks to referenced element, enable
`linkReferences` metadata option. This has no effect on LaTeX output,
since in this case, hyperlinking references is handled with `hyperref`
LaTeX package.

### Custom prefix per-reference

It's possible to provide your own prefix per-reference, f.ex.
`[Prefix @reference]` will replace default prefix (`fig.`/`sec.`/etc)
with prefix verbatim, e.g. `[Prefix @fig:1]` will be rendered as
`Prefix 1` instead of `fig. 1`.

In citation group, citations with the same prefix will be grouped. So,
for example `[A @fig:1; A @fig:2; B @fig:3]` will turn into
`A 1, 2, B 3`. It can be used to an advantage, although it's a bit more
cumbersome than it should be, e.g.
`[Appendices @sec:A1; Appendices @sec:A2; Appendices @sec:A3]` will turn
into `Appendices @A1-@A3` (with `@A1` and `@A3` being relevant section
numbers). Note that non-contiguous sequences of identical prefixes *will
not* be grouped.

***Not supported with cleveref LaTeX output.***

### Prefix suppression

Prepending `-` before `@`, like so `[-@citation]`, will suppress default
prefix, e.g. `[-@fig:1]` will produce just `1` (or whatever number it
happens to be) without `fig.` prefix.

In citation group, citations with and without prefixes will be in
different groups. So `[-@fig:1; @fig:2; -@fig:3]` will be rendered as
`1, fig. 2, 3`, so be careful with this feature. Again, non-contiguous
sequences are not grouped together.

## Lists

It's possible to use raw latex commands `\listoffigures`,
`\listoftables` and `\listoflistings`, which will produce ordered list of
figure/table/listings titles, in order of appearance in document.

For LaTeX output, `\listoflistings` depends on other options, and is defined in
preamble, so it will work reliably only with standalone/pdf output.

**NOTE:** With Pandoc 2.0.6 and up, you might have to explicitly separate
these commands if they are close together, at least when targeting
something besides LaTeX. So this might not work:

``` markdown
\listoffigures

\listoftables

\listoflistings
```

but this will:

``` markdown
\listoffigures

<!-- hack to split raw blocks -->

\listoftables

<!-- hack to split raw blocks -->

\listoflistings
```

With HTML-compatible output, lists are wrapped into a `div` with classes `list` and `list-of-<prefix>`, where `<prefix>` is either `fig`, `tbl` or `lst` depending on the type of the list. This allows for ad-hoc style overrides in HTML.

# Usage

Run pandoc with `--filter` option, passing path to pandoc-crossref
executable, or simply `pandoc-crossref`, if it's in PATH:

`pandoc --filter pandoc-crossref`

If you installed with cabal, it's most likely located in
`$HOME/.cabal/bin` on \*NIX systems, `$HOME/Library/Haskell/bin` on
Macs, or in `%AppData%\cabal\bin` on Windows.

## Customization

There are several parameters that can be set via YAML metadata (either
by passing `-M` to `pandoc`, or by setting it in source markdown)

A list of variables follows.

### General options

-   `cref`: if True, latex export will use `\cref` from cleveref
    package. Only relevant for LaTeX output. `\usepackage{cleveref}`
    will be automatically added to `header-includes`.
-   `chapters`: if True, number elements as `chapter.item`, and restart
    `item` on each first-level heading. You might also need to run pandoc with `--top-level-division=chapter` argument to signal it you want to use chapters; whether it's actually required or not depends on the output format, but it's always safe to include. Notice `chapters` and related options are ignored in LaTeX output. See [Note on LaTeX and `chapters` option](#note-on-latex-and-chapters-option)
-   `chaptersDepth`, default `1`: header level to treat as "chapter". If
    `chaptersDepth>1`, then items will be prefixed with several numbers,
    corresponding to header numbers, e.g. `fig. 1.4.3`.
-   `numberSections`, default `false`: if True, pandoc-crossref will
    prepend section number to section titles (as counted by
    pandoc-crossref itself). This also makes pandoc-crossref assign missing
    top-level headings the index of `1` instead of `0` to avoid ugly `sec.
    0.0.1` references.
-   `sectionsDepth`, default `0`:
    -   sectionsDepth \< 0 -- number all sections
    -   sectionsDepth == 0 -- be consistent with `chaptersDepths`
    -   sectionsDepth \> 0 -- number section levels up to and including
        `sectionsDepth`
-   `listings`: if True, generate code blocks for `listings` package.
    Only relevant for LaTeX output. `\usepackage{listings}` will be
    automatically added to `header-includes`. You need to specify
    `--listings` option as well.
-   `codeBlockCaptions`: if True, parse table-style code block captions.
-   `autoSectionLabels`, default `false`: Automatically prefix all
    section labels with `sec:`. Note that this messes with pandoc's
    automatic header references.
-   `autoEqnLabels`, default `false`: Automatically number all display
    equations (i.e. ones defined using `$$...$$`/`\[...\]`). Note that
    you won't be able to reference equations without explicit labels.
-   `tableEqns`, default `false`: Typeset equations and equation numbers
    as blocks instead of embedding numbers into equations themselves.
    Depending on output format, this might work better or worse.
    See also the section on [equation templates](#equation-templates).
-   `setLabelAttribute`, default `false`: set `label` attribute on objects to
    actual number used for referencing. This can be useful for post-processing.
-   `equationNumberTeX`, default `\\qquad`: use a LaTeX command for typesetting
    equation numbers. Remember that metadata is parsed as Markdown, so you may need to escape backslashes.
    This option doesn't affect LaTeX output (which offloads numbering to the LaTeX engine).

### Item title format

-   `figureTitle`, default `Figure`: Word(s) to prepend to figure
    titles, e.g. `Figure 1: Description`
-   `tableTitle`, default `Table`: Word(s) to prepend to table titles,
    e.g. `Table 1: Description`
-   `listingTitle`, default `Listing`: Word(s) to prepend to listing
    titles, e.g. `Listing 1: Description`
-   `titleDelim`, default `:`: What to put between object number and
    caption text.
-   `secHeaderDelim`, default ` ` (i.e. space): What to put between section
    number and title when `numberSections` is `true`. Can be a list,
    in that case it's indexed by heading level, 0-based.

#### Subfigure-specific

See [Subfigures](#subfigures)

-   `ccsDelim`, default `,&nbsp;`: delimiter for collected subfigure
    captions. See [Subfigures](#subfigures) and [Templates](#templates)
-   `ccsLabelSep`, default `&nbsp;—&nbsp;`: delimiter used between
    subfigure label and subfigure caption in collected captions. See
    [Subfigures](#subfigures) and [Templates](#templates)
-   `subfigGrid`, default `false`. If true, typeset subfigures inside a
    table. Ignored with LaTeX output. See [Subfigures](#subfigures)

### List titles

-   `lofTitle`, default `# List of Figures`: Title for list of figures
    (lof)
-   `lotTitle`, default `# List of Tables`: Title for list of tables
    (lot)
-   `lolTitle`, default `# List of Listings`: Title for list of listings
    (lol)

### List items

See also [List item templates](#list-item-templates)

-   `lofItemTitle`, default empty: Title each item in the list of figures, i.e. each item will be prefixed by this string.
-   `lotItemTitle`, default empty: Title each item in the list of tables, i.e. each item will be prefixed by this string.
-   `lolItemTitle`, default empty: Title each item in the list of listings, i.e. each item will be prefixed by this string.
-   `listItemTitleDelim`, default `.` (str "."): delimiter between list item numbers and captions.

### Reference format

-   `figPrefix`, default `fig.`, `figs.`: Prefix for references to
    figures, e.g. `figs. 1-3`
-   `eqnPrefix`, default `eq.`, `eqns.`: Prefix for references to
    equations, e.g. `eqns. 3,4`
-   `tblPrefix`, default `tbl.`, `tbls.`: Prefix for references to
    tables, e.g. `tbl. 2`
-   `lstPrefix`, default `lst.`, `lsts.`: Prefix for references to
    lists, e.g. `lsts. 2,5`
-   `secPrefix`, default `sec.`, `secs.`: Prefix for references to
    sections, e.g. `secs. 2,5`
-   `chapDelim`, default `.`: Delimiter between chapter number and item
    number.
-   `rangeDelim`, default `-`: Delimiter between reference ranges, e.g.
    `eq. 2-5`
-   `pairDelim`, default `,`: Delimiter between pair of reference
    ranges, e.g. `eq. 2-5 <and> 7-9`, or `eq. 2 <and> 7`, but
    `eq. 2, 4, 6`
-   `lastDelim`, default `,`: Delimiter between second-to-last and last
    reference ranges, e.g. `eq. 2-5, 6-8 <and> 10`. `pairDelim`
    overrides this for cases of exactly two references/ranges.
-   `refDelim`, default `,`: Delimiter between references, e.g.
    `eq. 2, 5, 7` or `eq. 2-4, 6-8`
-   `linkReferences`, default `false`: Make references hyperlinks to the
    referenced element
-   `nameInLink`, default `false`: For single-element references,
    inlcude prefix into hyperlink (when using `linkReferences`)

Note that none of the `*Delim` options are honored with cleveref output.
Use cleveref's customization options instead.

`figPrefix`, `eqnPrefix`, `tblPrefix`, `lstPrefix` can be YAML arrays.
That way, value at index corresponds to total number of references in
group, f.ex.

``` yaml
figPrefix:
  - "fig."
  - "figs."
```

Will result in all single-value references prefixed with "fig.", and all
reference groups of two and more will be prefixed with "figs.":

``` markdown
[@fig:one] -> fig. 1
[@fig:one; @fig:two] -> figs. 1, 2
[@fig:one; @fig:two; @fig:three] -> figs. 1-3
```

They can be YAML strings as well. In that case, prefix would be the same
regardless of number of references.

They can also be used with first character capitalized, i.e.
`FigPrefix`, etc. In this case, these settings will override default
reference capitailzation settings.

### Custom numbering

See [Custom Numbering Schemes](#custom-numbering-schemes)

-   `figLabels`, default `arabic`: the numbering scheme for figures.
-   `subfigLabels`, default `alpha a`: the numbering scheme for
    subfigures.
-   `eqLabels`, default `arabic`: the numbering scheme for equations.
-   `tblLabels`, default `arabic`: the numbering scheme for tables.
-   `lstLabels`, default `arabic`: the numbering scheme for listings.
-   `secLabels`, default `arabic`: the numbering scheme for sections.
-   `secLevelLabels`, default unset: the numbering scheme for sections, YAML array, indexed by heading level; will override `secLabels` if set.

### Item title templates

See [Templates](#templates)

-   `figureTemplate`, default
    `$$figureTitle$$ $$i$$$$titleDelim$$ $$t$$`: template for figure
    captions
-   `tableTemplate`, default `$$tableTitle$$ $$i$$$$titleDelim$$ $$t$$`:
    template for table captions
-   `listingTemplate`, default
    `$$listingTitle$$ $$i$$$$titleDelim$$ $$t$$`: template for listing
    captions
-   `secHeaderTemplate`, default `$$i$$$$secHeaderDelim[n]$$$$t$$`: template for
    section header
    text when `numberSections` is `true`

#### Subfigure templates

See [Subfigures](#subfigures)

-   `subfigureTemplate`, default
    `$$figureTitle$$ $$i$$$$titleDelim$$ $$t$$. $$ccs$$`: template for
    subfigure divs captions.
-   `subfigureChildTemplate`, default `$$i$$`: template for actual
    subfigure captions.
-   `ccsTemplate`, default `$$i$$$$ccsLabelSep$$$$t$$`: template for
    collected subfigure captions.

#### Equation templates

-   `eqnIndexTemplate`, default `($$i$$)`: used to adjust how equation index
    is rendered. This is required due to it being rendered inside a math block.
    Note that for the same reason formatting is mostly ignored.

-   `eqnInlineTemplate`, default `$$e$$$$equationNumberTeX$${$$i$$}`

    A template to typeset math when `tableEqns` is `false`. Similar to
    `eqnIndexTemplate`, formatting is mostly ignored, due to it being typeset
    inside a math environment. However, most LaTeX should work (but backslashes
    need to be doubled). The following template variables are known:

    - `e`, the equation itself,
    - `t`, the same as `e`, for backwards compatibility,
    - `i`, index after applying `eqnIndexTemplate`,
    - `nmi`, same as `i`, but see `eqnDisplayTemplate`,
    - `ri`, "raw" index, before applying `eqnIndexTemplate`,
    - `nmri`, here, same as `ri`, but see `eqnDisplayTemplate`.

    `eqnInlineTemplate` is ignored if `tableEqns` is `true`.

-   `eqnInlineTableTemplate`, default `$$e$$`

    A counterpart of `eqnInlineTemplate` for when `tableEqns` is `true`. Behaves
    the same. The logic is split like this mostly for backwards compatibility, but it also allows specifying unambiguous defaults.

    `eqnInlineTableTemplate` is ignored if `tableEqns` is `false`.

-   `eqnDisplayTemplate`, default `$$e$$`

    A template to typeset the math element produced by applying
    `eqnInlineTemplate` or `eqnInlineTableTemplate`.

    The output of this template must be a sequence of inline elements. If you
    want to produce block elements, see `eqnBlockTemplate`.

    The same template variables from `eqnInlineTemplate` are available, with the
    following changes:

    - `e` (and `t`) is now the formatted equation (as per `eqnInlineTemplate` or
      `eqnInlineTableTemplate`) wrapped in a math environment (display or inline
      depending on `eqnBlockInlineMath`),
    - `i` and `ri` are now wrapped in a math environment (same type as `e`).

    `eqnDisplayTemplate` is ignored if `tableEqns` is `true`.

-   `eqnBlockTemplate`, default

    ```markdown
    +:--------------------------------------------------------------:+-----:+
    | $$t$$                                                          | ```{=openxml}
    |                                                                | <w:tcPr><w:vAlign w:val="center"/></w:tcPr>
    |                                                                | ```
    |                                                                | $$i$$
    +----------------------------------------------------------------+-----+
    ```

    When used with `tableEqns`, a block to use to format equations. A table by
    default, but could be literally any block or a sequence of blocks. The
    behaviour is similar to `eqnDisplayTemplate`, but the elements produced are
    block elements (as opposed to inline).

    Note that the default contains a raw block to fix vertical alignment
    in docx output. If you're not targeting docx, it will be ignored by pandoc.

    `eqnBlockTemplate` is ignored if `tableEqns` is `false` (the default).

-   `eqnBlockInlineMath`, default `False`: if you need to use inline math while
    rendering equation templates. Useful, e.g., if you're using raw ooxml and
    tabstops to align equations in docx. For example,

    ```yaml
    tableEqns: true
    eqnBlockTemplate: |
       `<w:pPr><w:tabs><w:tab w:val="center" w:leader="none" w:pos="4680" /><w:tab w:val="right" w:leader="none" w:pos="9360" /></w:tabs></w:pPr><w:r><w:tab /></w:r>`{=openxml} $$t$$ `<w:r><w:tab /></w:r>`{=openxml} $$i$$
    eqnBlockInlineMath: true
    ```

    (tweak `w:pos` for the two tabstops to taste, the unit is 1/20th of an inch, sample values are appropriate for portrait A4 with 1-inch margins)

### Reference templates

See [Templates](#templates)

-   `figPrefixTemplate`, default `$$p$$&nbsp;$$i$$` -- figure reference
    template
-   `eqnPrefixTemplate`, default `$$p$$&nbsp;$$i$$` -- equation
    reference template
-   `tblPrefixTemplate`, default `$$p$$&nbsp;$$i$$` -- table reference
    template
-   `lstPrefixTemplate`, default `$$p$$&nbsp;$$i$$` -- listing reference
    template
-   `secPrefixTemplate`, default `$$p$$&nbsp;$$i$$` -- section reference
    template
-   `refIndexTemplate`, default `$$i$$$$suf$$` -- individual reference
    index template
-   `subfigureRefIndexTemplate`, default `$$i$$$$suf$$ ($$s$$)` --
    subfigure reference index template

### List item templates

See [Templates](#templates)

-   `lofItemTemplate`, default `$$lofItemTitle$$$$i$$$$listItemTitleDelim$$ $$t$$\\\n` -- list-of-figures item template
-   `lotItemTemplate`, default `$$lotItemTitle$$$$i$$$$listItemTitleDelim$$ $$t$$\\\n`)
-   `lolItemTemplate`, default `$$lolItemTitle$$$$i$$$$listItemTitleDelim$$ $$t$$\\\n`)

Special handling is enabled for templates that are either ordered or bullet lists: items will be merged into a single list. Thus, for example, given

```yaml
lofItemTemplate: |
  1. $$t$$
```

the list of figures will be formatted as

```markdown
# List of Figures

::: {.list .list-of-fig}
1.  Figure 1 caption
2.  Figure 2 caption
3.  Figure 3 caption
4.  ...
:::
```

### LaTeX customization

Support for above variables with LaTeX/PDF output is limited. In
particular, the following variables are honored:

-   `figureTitle`
-   `tableTitle`
-   `listingTitle`
-   `lofTitle` -- ignores formatting
-   `lotTitle` -- ignores formatting
-   `lolTitle` -- ignores formatting
-   `*Prefix`, upper-/lowercase and single/plural form. Note that with
    cleveref output, if `*Prefix` is array, only first two items are
    used, and the rest is ignored.

Templates are *not* supported.

You can add arbitrary LaTeX commands to document header, however, using
`header-includes` metadata field. Please bear in mind, that
pandoc-crossref up to and including 0.1.2.1 requires `header-includes`
to be YAML array, e.g.

``` yaml
header-includes:
    - "\\newcommand{\\pcdoc}{Pandoc-crossref documentation}"
```

This will be added *before* any customization applied by
pandoc-crossref. For a complete list of what is added to template,
consult
[ModifyMeta.hs](https://github.com/lierdakil/pandoc-crossref/blob/master/lib/Text/Pandoc/CrossRef/Util/ModifyMeta.hs).

## Templates

pandoc-crossref supports advanced caption customization via caption
templates. Templates are specified as YAML metadata variables (see
[Customization](#customization)), and are parsed as default Pandoc
Markdown. Variables are specified with display math syntax, i.e.
`$$var$$` in a template will be replaced with value of variable `var`.
Variables can be specified in YAML metadata block, or from command line
(with `-M` switch). There are two special variables, that are set
internally:

-   `i` -- object number, possibly with chapter number (if
    `chapter=True`)
-   `t` -- object caption, as given in source Markdown

Also there is a number of specific variables that are meaningful only in
certain contexts:

-   `ccs` -- collected subfigure captions. Only applicable to
    `subfigureTemplate`. Collected captions will be separated by
    `ccsDelim` and individual captions will be printed with
    `ccsTemplate`. See [Subfigures](#subfigures)
-   `suf` -- reference suffix, applicable to `refIndexTemplate`,
    `subfigureRefIndexTemplate`
-   `s` -- subfigure index, applicable to `subfigureRefIndexTemplate`

`xPrefixTemplate`, where `x` is `fig`, `eqn`, etc, are a special case.
Those don't have `t` variable, since there is no caption in source
markdown, but instead have `p` variable, that binds to relevant
`xPrefix`. This is done this way, since actual prefix value can depend
on `i`. In `xPrefixTemplate`, `i` references formatted object numbers, i.e. if given a list of references like `[@fig:1; @fig:2; @fig:3]`, here `i` will contain something like `1-3`.

`refIndexTemplate` is the template for the individual reference index. It can be either a plain template, or can be a YAML object with keys corresponding to different prefixes, and a special key `default` used as a fallback, e.g.

```yaml
refIndexTemplate:
  sec: $$i$$$$suf$$ ($$t$$)
  default: $$i$$$$suf$$
```

`refIndexTemplate` has the following internal variables defined:

- `i` -- formatted object index (possibly with chapter number)
- `suf` -- literal suffix used in the reference, e.g. given `[@fig:1 some suffix]`, `suf` will contain literally ` some suffix` (complete with the leading space)
- `t` -- object title, if any, or empty if the object has no title

`subfigureRefIndexTemplate` is roughly the same as `refIndexTemplate` but is used specifically for subfigures. It additionally has `s` variable defined, which is described above.

Additionally, a special syntax is provided for indexed access to array metadata variables: `arrayVariable[indexVariable]`, where `arrayVariable` is an array-like metadata variable, and `indexVariable` is an integer-typed template variable.
If `indexVariable` is larger than length of `arrayVariable`, then the last
element in `arrayVariable` is used.

Indexed access can be useful with `secHeaderTemplate` for example, where you
might want to add a custom prefix depending on the header level.

For example, with this YAML metadata:

``` yaml
secHeaderTemplate: $$secHeaderPrefix[n]$$$$i$$. $$t$$
secHeaderPrefix:
  - "Chapter&#32;"
  - "Section&#32;"
  - ""
sectionsDepth: -1
numberSections: true
```

top-level sections will be prefixed with `Chapter `, second-level sections will
be prefixed with `Section ` and the rest won't be prefixed with anything.

Please note that at the moment, templates are not supported with
LaTeX/PDF output.

## Custom Numbering Schemes

It's possible to use other numbering schemes apart from arabic. This is
controlled by several metadata options, consult
[Customization](#customization) for a list. Possible values are:

-   `arabic` -- arabic numbers (1, 2, 3 ...)
-   `roman` -- roman numbers (I, II, III, IV, ...)
-   `lowercase roman` -- lowercase roman numbers (i, ii, iii, iv, ...)
-   `alpha x`, where `x` is first letter to start from. This will work
    for any letter, but will use UTF-8 codepage to determine what's
    next, so using something strange is not advised. For example, you
    can safely use `alpha a` to get lowercase latin letters for 26
    figures. After that, it will get weird (since basic latin alphabet
    has 26 letters). Specifically, it will go into characters space
    (`{`, `|`, `}`, etc). You can consult
    http://www.fileformat.info/info/unicode/block/basic\_latin/utf8test.htm
    for general idea on letter progression.
-   list of strings. You can define a YAML array for numbers. Mapping is
    1:1. For example, `figLabels: [α, β, γ, δ, ε]`
    will give first object label 'α', second -- 'β', etc, up until the fifths.

    Note that it will repeat last item in list indefinitely if there are
    more references than items in list, i.e. in the example above, sixths object and all after that will also have label 'ε'.

## Settings file

It is also possible to set variables used by pandoc-crossref with a
separate YAML file. If a given variable is not set in metadata, then
pandoc-crossref will attempt to read it from file specified by
`crossrefYaml` metadata variable, or, if not set, from
`pandoc-crossref.yaml` from current working directory. This allows for
reusable configurations. One possible application is ad-hoc
internationalization.

For example, consider `$HOME/misc/pandoc-crossref-es.yaml`:

``` yaml
figureTitle: "Figura"
tableTitle: "Tabla"
figPrefix: "fig."
eqnPrefix: "ec."
tblPrefix: "tbl."
loftitle: "# Lista de figuras"
lotTitle: "# Lista de tablas"
```

pandoc-crossref will send this data to pandoc wrapped in lines of `---`.
The YAML file's first line should specify a variable; it will not pass
the variables if it is `---` or a blank line.

One could use this with pandoc-crossref as follows:

`pandoc -F pandoc-crossref.hs -M "crossrefYaml=$HOME/misc/pandoc-crossref-es.yaml"`

You can also use global configuration files, which are expected in
`$HOME/.pandoc-crossref/config.yaml` and
`$HOME/.pandoc-crossref/config-$FORMAT.yaml`, where `$FORMAT` is output
format, f.ex. `latex` or `epub`. On Windows, `$HOME` in general resolves
to user's root directory, e.g. `C:\Users\username\`.

Priorities are as follows (from highest to lowest):

-   document metadata
-   `crossrefYaml`/`$CWD/pandoc-crossref.yaml`
-   `$HOME/.pandoc-crossref/config-$FORMAT.yaml`
-   `$HOME/.pandoc-crossref/config.yaml`

# License

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

This program includes code from roman-numerals library, covered by the following terms:

> Copyright 2009–2014 Roel van Dijk
>
> All rights reserved.
>
> Redistribution and use in source and binary forms, with or without
> modification, are permitted provided that the following conditions are
> met:
>
>     * Redistributions of source code must retain the above copyright
>       notice, this list of conditions and the following disclaimer.
>
>     * Redistributions in binary form must reproduce the above
>       copyright notice, this list of conditions and the following
>       disclaimer in the documentation and/or other materials provided
>       with the distribution.
>
>     * The names of contributors may not be used to endorse or promote
>       products derived from this software without specific prior
>       written permission.
>
> THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
> "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
> LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
> A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
> OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
> SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
> LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
> DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
> THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
> (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
> OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
