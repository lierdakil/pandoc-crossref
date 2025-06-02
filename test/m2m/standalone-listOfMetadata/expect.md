---
autoEqnLabels: false
autoSectionLabels: false
ccsDelim: ", "
ccsLabelSep: " --- "
ccsTemplate: $$i$$$$ccsLabelSep$$$$t$$
chapDelim: .
chapters: false
chaptersDepth: 1
codeBlockCaptions: true
cref: false
crossrefYaml: pandoc-crossref.yaml
eqLabels: arabic
eqnBlockInlineMath: false
eqnBlockTemplate: |
  +:-------------------------------------------------------------:+------:+
  | $$t$$                                                         | $$i$$ |
  +---------------------------------------------------------------+-------+
eqnDisplayTemplate: $$e$$
eqnIndexTemplate: ($$i$$)
eqnInlineTableTemplate: $$e$$
eqnInlineTemplate: $$e$$$$equationNumberTeX$${$$i$$}
eqnPrefix:
- eq.
- eqns.
eqnPrefixTemplate: $$p$$ $$i$$
equationNumberTeX: \\qquad
figLabels: arabic
figPrefix:
- fig.
- figs.
figPrefixTemplate: $$p$$ $$i$$
figureTemplate: $$figureTitle$$ $$i$$$$titleDelim$$ $$t$$
figureTitle: Figure
lastDelim: ", "
linkReferences: false
list-of-figures: |
  # List of Figures

  ::: {.list .list-of-fig}
  1\. 1\

  2\. 2\

  3\. 3\

  4\. 4\

  5\. 5\

  6\. 6\

  7\. 7\

  8\. 8\

  9\. 9\
  :::
list-of-listings: |
  # List of Listings

  ::: {.list .list-of-lst}
  1\. Listing caption 1\

  2\. Listing caption 2\

  3\. Listing caption 3\

  4\. Listing caption 4\
  :::
list-of-tables: |
  # List of Tables

  ::: {.list .list-of-tbl}
  1\. My table\

  2\. Table\
  :::
listings: false
listingTemplate: $$listingTitle$$ $$i$$$$titleDelim$$ $$t$$
listingTitle: Listing
listItemTitleDelim: .
listOfMetadata: true
lofItemTemplate: |
  $$lofItemTitle$$$$i$$$$listItemTitleDelim$$ $$t$$\
lofTitle: |
  # List of Figures
lolItemTemplate: |
  $$lolItemTitle$$$$i$$$$listItemTitleDelim$$ $$t$$\
lolTitle: |
  # List of Listings
lotItemTemplate: |
  $$lotItemTitle$$$$i$$$$listItemTitleDelim$$ $$t$$\
lotTitle: |
  # List of Tables
lstLabels: arabic
lstPrefix:
- lst.
- lsts.
lstPrefixTemplate: $$p$$ $$i$$
nameInLink: false
numberSections: false
pairDelim: ", "
rangeDelim: "-"
refDelim: ", "
refIndexTemplate: $$i$$$$suf$$
secHeaderDelim:
secHeaderTemplate: $$i$$$$secHeaderDelim[n]$$$$t$$
secLabels: arabic
secPrefix:
- sec.
- secs.
secPrefixTemplate: $$p$$ $$i$$
sectionsDepth: 0
standalone: true
subfigGrid: false
subfigLabels: alpha a
subfigureChildTemplate: $$i$$
subfigureRefIndexTemplate: $$i$$$$suf$$ ($$s$$)
subfigureTemplate: $$figureTitle$$ $$i$$$$titleDelim$$ $$t$$. $$ccs$$
tableEqns: false
tableTemplate: $$tableTitle$$ $$i$$$$titleDelim$$ $$t$$
tableTitle: Table
tblLabels: arabic
tblPrefix:
- tbl.
- tbls.
tblPrefixTemplate: $$p$$ $$i$$
titleDelim: ":"
---

![Figure 1: 1](fig1.png){#fig:1}

![Figure 2: 2](fig2.png){#fig:2}

![Figure 3: 3](fig3.png){#fig:3}

![Figure 4: 4](fig4.png){#fig:4}

![Figure 5: 5](fig5.png){#fig:5}

![Figure 6: 6](fig6.png){#fig:6}

![Figure 7: 7](fig7.png){#fig:7}

![Figure 8: 8](fig8.png){#fig:8}

![Figure 9: 9](fig9.png){#fig:9}

::: {#lst:code1 .listing .haskell}
Listing 1: Listing caption 1

``` haskell
main :: IO ()
main = putStrLn "Hello World!"
```
:::

::: {#lst:code2 .listing .haskell}
Listing 2: Listing caption 2

``` haskell
main :: IO ()
main = putStrLn "Hello World!"
```
:::

::: {#lst:code3 .listing .haskell}
Listing 3: Listing caption 3

``` haskell
main :: IO ()
main = putStrLn "Hello World!"
```
:::

::: {#lst:code4 .listing .haskell}
Listing 4: Listing caption 4

``` haskell
main :: IO ()
main = putStrLn "Hello World!"
```
:::

------------------------------------------------------------------------

::: {#tbl:mytable}
  a   b   c
  --- --- ---
  1   2   3
  4   5   6

  : Table 1: My table
:::

::: {#tbl:1}
  a   b
  --- ---
  1   2

  : Table 2: Table
:::

# List of Figures

::: {.list .list-of-fig}
1\. 1\

2\. 2\

3\. 3\

4\. 4\

5\. 5\

6\. 6\

7\. 7\

8\. 8\

9\. 9\
:::

# List of Tables

::: {.list .list-of-tbl}
1\. My table\

2\. Table\
:::

# List of Listings

::: {.list .list-of-lst}
1\. Listing caption 1\

2\. Listing caption 2\

3\. Listing caption 3\

4\. Listing caption 4\
:::
