# pandoc-crossref filter

pandoc-crossref is a pandoc filter for numbering figures, equations and cross-references to them.

This version of pandoc-eqnos was tested using pandoc 1.13.2.

This work is inspired by [pandoc-fignos][1] and [pandoc-eqnos][2] by @tomduck.

[1]: https://github.com/tomduck/pandoc-fignos
[2]: https://github.com/tomduck/pandoc-eqnos

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

### References

```markdown
[@fig:label1;@fig:label2;...] or [@eq:label1;@eq:label2;...] or @fig:label or @eq:label
```

Reference syntax heavily relies on citation syntax
