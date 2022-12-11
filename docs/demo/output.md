This is a demo file for pandoc-crossref. With this filter, you can
cross-reference figures (see figs. 2-4), display equations (see eq. 1),
tables (see tbl. 1) and sections (secs. 1, 2, 4.1-4.3)

For immediate example, see fig. 1

![Figure \# 1: A figure](img1.jpg){#fig:figure0}

There is also support for code blocks, for example, lsts. 1-3

It's possible to capitalize reference prefixes, like this: Fig. 2.

In case of multiple references, capitalization is determined by first
reference. Figs. 2, 3 is capitalized, while figs. 2, 3 is not.

It is also possible to mix different references, like fig. 2, tbl. 1,
lsts. 1, 2, figs. 3, 4, which will be grouped in order they are
specified. You can even intermix this with regular citations, although
it's not recommended: fig. 2, tbl. 1, [@unprocessedCitation]

You can also have custom chapter reference labels, like
sec. AppA.CustLab

Subfigures are supported, see figs. 5, 5 (b)

# Chapter 1. Figures {#sec:sec1}

![Figure \# 2: First figure](img1.jpg){#fig:figure1}

![Figure \# 3: Second figure](img2.jpg){#fig:figure2}

![Figure \# 4: Third figure](img3.jpg){#fig:figure3}

![Unlabelled image](img1.jpg)

::: {#fig:subfigures .subfigures}
![a](img1.jpg)

![b](img1.jpg){#fig:subfigureB}

Figure \# 5: Subfigures caption. a --- Subfigure a, b --- Subfigure b
:::

# Chapter 2. Equations {#sec:sec2}

Display equations are labelled and numbered

[$$ P_i(x) = \sum_i a_i x^i \qquad{(1)}$$]{#eq:eqn1}

Since 0.1.6.0 those can also appear in the middle of paragraph
[$$a x^2 + b x^2 + c = 0\qquad{(2)}$$]{#eq:quadr} like this.

# Chapter 3. Tables {#sec:chapter-3.-tables}

::: {#tbl:table1}
  First Header   Second Header
  -------------- ---------------
  Content Cell   Content Cell
  Content Cell   Content Cell

  : *Table 1*: Table example
:::

Table without caption:

  First Header   Second Header
  -------------- ---------------
  Content Cell   Content Cell
  Content Cell   Content Cell

# Chapter 4. Code blocks {#sec:chapter-4.-code-blocks}

There are a couple options for code block labels. Those work only if
code block id starts with `lst:`, e.g. `{#lst:label}`

## `caption` attribute {#sec:caption-attr}

`caption` attribute will be treated as code block caption. If code block
has both id and `caption` attributes, it will be treated as numbered
code block.

::: {#lst:captionAttr .listing .haskell}
Listing 1: Listing caption

``` haskell
main :: IO ()
main = putStrLn "Hello World!"
```
:::

```{=tex}
\pagebreak
```
## Table-style captions {#sec:table-capts}

Enabled with `codeBlockCaptions` metadata option. If code block is
immediately adjacent to paragraph, starting with `Listing:` or `:`, said
paragraph will be treated as code block caption.

::: {#lst:tableCaption .listing .haskell}
Listing 2: Listing caption

``` haskell
main :: IO ()
main = putStrLn "Hello World!"
```
:::

## Wrapping div {#sec:wrapping-div}

Wrapping code block without label in a div with id `lst:...` and class,
starting with `listing`, and adding paragraph before code block, but
inside div, will treat said paragraph as code block caption.

::: {#lst:wrappingDiv .listing .haskell}
Listing 3: Listing caption

``` haskell
main :: IO ()
main = putStrLn "Hello World!"
```
:::

# Unnumbered chapter. {#sec:unnumbered-chapter. .unnumbered}

This chapter doesn't change chapter prefix of referenced elements,
instead keeping number of previous chapter, e.g.
[$$ S(x) = \int_{x_1}^{x_2} a x+b \  \mathrm{d}x \qquad{(3)}$$]{#eq:eqn2}

# Chapter 5. Reference lists {#sec:chapter-5.-reference-lists}

It's also possible to show lists of figures and tables, like this:

## List of Figures {#list-of-figures}

::: {.list .list-of-fig}
1\. A figure\

2\. First figure\

3\. Second figure\

4\. Third figure\

5\. Subfigures caption\

5 (a). Subfigure a\

5 (b). Subfigure b\
:::

## List of Tables {#list-of-tables}

::: {.list .list-of-tbl}
1\. Table example\
:::

## List of Listings {#list-of-listings}

::: {.list .list-of-lst}
1\. Listing caption\

2\. Listing caption\

3\. Listing caption\
:::

# Appendix A. Custom labels {#sec:appendix-a.-custom-labels label="AppA"}

## This section will have custom label {#sec:custlabs label="CustLab"}
