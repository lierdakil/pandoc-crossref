# Chapter 1

![Figure 1: first figure caption with a citation to fig. 2 in
it](fig1.jpg){#fig:fig1}

Some filler text, with in-text citations of both fig. 1 and fig. 2, and
also (functionally) citing tbl. 1

![Figure 2: second figure caption with a citation to fig. 1 in
it](fig2.jpg){#fig:fig2}

There's actually a new problem where it breaks *other* valid references
if you try to cite them in main text next to one of the ones cited in a
figure caption, e.g. (fig. 1, tbl. 1) or (fig. 2, tbl. 1), although if
you cite the thing alone it'll work just fine (tbl. 1)

::: {#tbl:tbl1}
  col1   col2
  ------ ------
  row1   row1
  row2   ro2

  : Table 1: Table 1
:::

# List of Figures

::: {.list .list-of-fig}
1\. first figure caption with a citation to fig. 2 in it\

2\. second figure caption with a citation to fig. 1 in it\
:::
