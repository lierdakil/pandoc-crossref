You can define subfigures:

:::: {#fig:subfigures .figure .subfigures}
![a](fig1.png){#fig:subfig1} ![b](fig2.png){#fig:subfig2} ![c](fig3.png)

![d](fig4.png){#fig:subfig4} ![e](fig5.png) ![f](fig6.png){#fig:subfig6}

![g](fig7.png){#fig:subfig7} ![h](fig8.png) ![i](fig9.png){#fig:subfig9}

::: caption
Figure 1: Caption. a --- 1, b --- 2, c --- 3, d --- 4, e --- 5, f --- 6,
g --- 7, h --- 8, i --- 9
:::
::::

:::: {#fig:subfigures2 .figure .subfigures}
![a](fig1.png){#fig:subfig21}

![b](fig2.png){#fig:subfig22}

![c](fig3.png)

![d](fig4.png){#fig:subfig24}

![e](fig5.png)

![f](fig6.png){#fig:subfig26}

![g](fig7.png){#fig:subfig27}

![h](fig8.png)

![i](fig9.png){#fig:subfig29}

::: caption
Figure 2: Caption. a --- 1, b --- 2, c --- 3, d --- 4, e --- 5, f --- 6,
g --- 7, h --- 8, i --- 9
:::
::::

Figures themselves can be referenced fig. 2, as well as individual
subfigures: figs. 1 (a), 1 (b), 2 (i)
