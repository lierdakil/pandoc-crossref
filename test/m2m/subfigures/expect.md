You can define subfigures:

::: {#fig:subfigures .figure .subfigures}
![a](fig1.png){#fig:subfig1} ![b](fig2.png){#fig:subfig2} ![c](fig3.png)

![d](fig4.png){#fig:subfig4} ![e](fig5.png) ![f](fig6.png){#fig:subfig6}

![g](fig7.png){#fig:subfig7} ![h](fig8.png) ![i](fig9.png){#fig:subfig9}

::: caption
Figure 1: Caption. a --- 1, b --- 2, c --- 3, d --- 4, e --- 5, f --- 6,
g --- 7, h --- 8, i --- 9
:::
:::

::: {#fig:subfigures2 .figure .subfigures}
::: {#fig:subfig21 .figure}
![1](fig1.png)

::: caption
a
:::
:::

::: {#fig:subfig22 .figure}
![2](fig2.png)

::: caption
b
:::
:::

::: figure
![3](fig3.png)

::: caption
c
:::
:::

::: {#fig:subfig24 .figure}
![4](fig4.png)

::: caption
d
:::
:::

::: figure
![5](fig5.png)

::: caption
e
:::
:::

::: {#fig:subfig26 .figure}
![6](fig6.png)

::: caption
f
:::
:::

::: {#fig:subfig27 .figure}
![7](fig7.png)

::: caption
g
:::
:::

::: figure
![8](fig8.png)

::: caption
h
:::
:::

::: {#fig:subfig29 .figure}
![9](fig9.png)

::: caption
i
:::
:::

::: caption
Figure 2: Caption. a --- 1, b --- 2, c --- 3, d --- 4, e --- 5, f --- 6,
g --- 7, h --- 8, i --- 9
:::
:::

Figures themselves can be referenced fig. 2, as well as individual
subfigures: figs. 1 (a), 1 (b), 2 (i)
