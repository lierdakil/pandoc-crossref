# pandoc-crossref filter

pandoc-crossref is a pandoc filter for numbering figures, equations, tables and cross-references to them.

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

### Equation labels

```markdown
$$ math $$ {#eq:label}
```

To label a display equation, append `{#eq:label}` (with `label` being something unique to reference this equation by) immediately after math block.

This only works if display math and label specification are in a paragraph of its own.

### Table labels

```markdown
a   b   c
--- --- ---
1   2   3
4   5   6

: Caption {#tbl:label}
```

To label a table, append `{#tbl:label}` at the end of table caption (with `label` being something unique to reference this equation by)

### References

```markdown
[@fig:label1;@fig:label2;...] or [@eq:label1;@eq:label2;...] or [@tbl:label1;@tbl:label2;...] or @fig:label or @eq:label or @tbl:label
```

Reference syntax heavily relies on citation syntax. Basic reference is created by writing `@`, then basically desired label with prefix. It is also possible to reference a group of objects *of the same type*, by putting them into brackets with `;` as separator. Sequential reference numbers will be shortened, e.g. `1,2,3` will be shortened to `1-3`.

## Usage

Get `pandoc-crossref.hs` at your convenience and run pandoc with `--filter` option:

`pandoc --filter ./pandoc-crossref.hs`.

### Customization

There are several parameters that can be set via YAML metadata (either by passing `-M` to `pandoc`, or by setting it in source markdown)

Following variables are supported:

* `cref`: if present, latex export will use `\cref` from cleveref package. It is user's responsibility to include relevant `\usepackage` directives in template
* `figureTitle`, default `Figure`: Word to prepend to figure titles, e.g. `Figure 1: Description`
* `tableTitle`, default `Table`: Word to prepend to table titles, e.g. `Table 1: Description`
* `titleDelimiter`, default `:`: What to put between object number and caption text.
* `figPrefix`, default `fig.`: Prefix for references to figures, e.g. `fig. 1-3`
* `eqnPrefix`, default `eq.`: Prefix for references to equations, e.g. `eq. 3,4`
* `tblPrefix`, default `tbl.`: Prefix for references to tables, e.g. `tbl. 2`
