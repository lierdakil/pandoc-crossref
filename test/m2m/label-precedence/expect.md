# \* First Section {#first-section label="*"}

text

::: {#fig:fig1 .figure}
![A figure](image.png)

::: caption
Figure α: A figure
:::
:::

## \*.A Subsection {#subsection}

other text

::: {#fig:fig2 .figure}
![A figure with custom label](image.png){label="+"}

::: caption
Figure +: A figure with custom label
:::
:::

### \*.A.A Subsubsection {#subsubsection}

text text text

# B Custom on other elements {#custom-on-other-elements}

::: {#fig:fig3 .figure}
![Figure](fig.png){label="F"}

::: caption
Figure F: Figure
:::
:::

::: {#tbl:table label="T"}
  a   b   c
  --- --- ---
  1   2   3
  4   5   6

  : Table T: Caption
:::

[$$y = e^x\qquad{(E)}$$]{#eq:equation label="E"}

fig. F tbl. T eq. E
