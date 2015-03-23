---
cref: False
figureTitle: "Figure \\#"
lofTitle: "# List of Figures with custom title"
---

This is a demo file for pandoc-crossref. With this filter, you can cross-reference figures (see [@fig:figure1;@fig:figure2;@fig:figure3]), display equations (see @eq:eqn1) and tables (see [@tbl:table1])

![First figure](img1.jpg){#fig:figure1}

![Second figure](img2.jpg){#fig:figure2}

![Third figure](img3.jpg){#fig:figure3}

![Unlabelled image](img1.jpg)

$$ P_i(x) = \sum_i a_i x^i $$ {#eq:eqn1}

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

It's also possible to show lists of figures and tables, like this:

\listoffigures

\listoftables
