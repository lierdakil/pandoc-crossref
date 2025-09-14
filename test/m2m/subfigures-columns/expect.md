You can define subfigures:

:::::::::::::::: {#fig:subfigures .figure .subfigures}
:::::: columns
::: {.column width="33.0%"}
![a](fig1.png){#fig:subfig1}
:::

::: {.column width="33.0%"}
![b](fig2.png){#fig:subfig2}
:::

::: {.column width="33.0%"}
![c](fig3.png)
:::
::::::

:::::: columns
::: {.column width="33.0%"}
![d](fig4.png){#fig:subfig4}
:::

::: {.column width="33.0%"}
![e](fig5.png)
:::

::: {.column width="33.0%"}
![f](fig6.png){#fig:subfig6}
:::
::::::

:::::: columns
::: {.column width="33.0%"}
![g](fig7.png){#fig:subfig7}
:::

::: {.column width="33.0%"}
![h](fig8.png)
:::

::: {.column width="33.0%"}
![i](fig9.png){#fig:subfig9}
:::
::::::

::: caption
Figure 1: Caption. a --- 1, b --- 2, c --- 3, d --- 4, e --- 5, f --- 6,
g --- 7, h --- 8, i --- 9
:::
::::::::::::::::

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
