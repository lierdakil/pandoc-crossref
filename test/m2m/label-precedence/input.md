---
numberSections: true
sectionsDepth: -1
secLabels: roman
secLevelLabels:
  - alpha A
figLabels: alpha α
tblLabels: alpha α
eqLabels: alpha α
---

# First Section {label="*"}

text

![A figure](image.png){#fig:fig1}

## Subsection

other text

![A figure with custom label](image.png){#fig:fig2 label="+"}

### Subsubsection

text text text

# Custom on other elements

![Figure](fig.png){#fig:fig3 label="F"}

:::{#tbl:table label="T"}
a   b   c
--- --- ---
1   2   3
4   5   6

: Caption
:::

[$$y = e^x$$]{#eq:equation label="E"}

@fig:fig3 @tbl:table @eq:equation
