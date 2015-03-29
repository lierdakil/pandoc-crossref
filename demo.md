---
cref: False
figureTitle: "Figure \\#"
lofTitle: "## List of Figures"
lotTitle: "## List of Tables"
tableTemplate: "*$$tableTitle$$ $$i$$*$$titleDelim$$ $$t$$"
---

This is a demo file for pandoc-crossref. With this filter, you can cross-reference figures (see [@fig:figure1;@fig:figure2;@fig:figure3]), display equations (see @eq:eqn1) and tables (see [@tbl:table1])

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

# Chapter 4. Reference lists

It's also possible to show lists of figures and tables, like this:

\listoffigures

\listoftables
