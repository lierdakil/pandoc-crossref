---
cref: False
codeBlockCaptions: True
figureTitle: "Figure \\#"
lofTitle: "## List of Figures"
lotTitle: "## List of Tables"
tableTemplate: "*$$tableTitle$$ $$i$$*$$titleDelim$$ $$t$$"
---

This is a demo file for pandoc-crossref. With this filter, you can cross-reference figures (see [@fig:figure1;@fig:figure2;@fig:figure3]), display equations (see @eq:eqn1) and tables (see [@tbl:table1])

There is also support for code blocks, for example, [@lst:captionAttr; @lst:tableCaption; @lst:wrappingDiv]

It's also possible to capitalize reference prefixes, like this: [@Fig:figure1].

In case of multiple references, capitalization is determined by first reference. [@Fig:figure1; @fig:figure2] is capitalized, while [@fig:figure2; @Fig:figure1] is not.

# Chapter 1. Figures

![First figure](img1.jpg){#fig:figure1}

![Second figure](img2.jpg){#fig:figure2}

![Third figure](img3.jpg){#fig:figure3}

![Unlabelled image](img1.jpg)

# Chapter 2. Equations

$$ P_i(x) = \sum_i a_i x^i $$ {#eq:eqn1}

# Chapter 3. Tables

| First Header | Second Header |
|:-------------|:--------------|
| Content Cell | Content Cell  |
| Content Cell | Content Cell  |

: Table example {#tbl:table1}

Table without caption:

| First Header | Second Header |
|:-------------|:--------------|
| Content Cell | Content Cell  |
| Content Cell | Content Cell  |

# Chapter 4. Code blocks

There are a couple options for code block labels. Those work only if code block id starts with `lst:`, e.g. `{#lst:label}`

## `caption` attribute

`caption` attribute will be treated as code block caption. If code block has both id and `caption` attributes, it will be treated as numbered code block.

```{#lst:captionAttr .haskell caption="Listing caption"}
main :: IO ()
main = putStrLn "Hello World!"
```

\pagebreak

## Table-style captions

Enabled with `codeBlockCaptions` metadata option. If code block is immediately
adjacent to paragraph, starting with `Listing: ` or `: `, said paragraph will be
treated as code block caption.

Listing: Listing caption

```{#lst:tableCaption .haskell}
main :: IO ()
main = putStrLn "Hello World!"
```

## Wrapping div

Wrapping code block without label in a div with id `lst:...` and class, starting with `listing`, and adding paragraph before code block, but inside div, will treat said paragraph as code block caption.

<div id="lst:wrappingDiv" class="listing">
Listing caption
```{.haskell}
main :: IO ()
main = putStrLn "Hello World!"
```
</div>

# Chapter 5. Reference lists

It's also possible to show lists of figures and tables, like this:

\listoffigures

\listoftables

\listoflistings
