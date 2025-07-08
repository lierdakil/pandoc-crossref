# Chapter 1

![first figure caption with a citation to [@fig:fig2] in it](fig1.jpg){#fig:fig1}

Some filler text, with in-text citations of both [@fig:fig1] and [@fig:fig2], and also (functionally) citing [@tbl:tbl1]

![second figure caption with a citation to [@fig:fig1] in it](fig2.jpg){#fig:fig2}

There's actually a new problem where it breaks *other* valid references if you try to cite them in main text next to one of the ones cited in a figure caption, e.g. ([@fig:fig1, @tbl:tbl1]) or ([@fig:fig2, @tbl:tbl1]), although if you cite the thing alone it'll work just fine ([@tbl:tbl1])


| col1 | col2 |
|------|------|
| row1 | row1 |
| row2 | ro2  |

: Table 1 {#tbl:tbl1}


\listoffigures
