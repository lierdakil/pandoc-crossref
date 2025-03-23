# main section {#main-section .unnumbered}

blah blah blah figs. 1, S.1

![Figure 1: first figure](figs/fig1.jpg){#fig:fig1}

furthermore, etc., (figs. 2, S.2)

![Figure 2: second figure](figs/fig2.jpg){#fig:fig2}

and finally (fig. 3)

![Figure 3: third figure](figs/fig3.jpg){#fig:fig3}

# S hidden supplemental section {#hidden-supplemental-section label="S"}

:::: {#fig:fig_s1 .figure}
![Figure S.1: hidden! first supp figure](figs/fig_s1.jpg){hidden="y"}

::: {.caption short-caption=""}
Figure S.1: hidden! first supp figure
:::
::::

:::: {#fig:fig_s2 .figure}
![Figure S.2: hidden! second supp figure](figs/fig_s2.jpg){hidden="y"}

::: {.caption short-caption=""}
Figure S.2: hidden! second supp figure
:::
::::

# 2 hidden section {#hidden-section hidden="y"}

:::: {#fig:fig_h1 .figure}
![Figure 2.1: hidden! first hidden figure](figs/fig_s1.jpg)

::: {.caption short-caption=""}
Figure 2.1: hidden! first hidden figure
:::
::::

:::: {#fig:fig_h2 .figure}
![Figure 2.2: hidden! second hidden figure](figs/fig_s2.jpg)

::: {.caption short-caption=""}
Figure 2.2: hidden! second hidden figure
:::
::::

## 2.1 subsection of a hidden section

:::: {#fig:fig_h1_s .figure}
![Figure 2.1.1: hidden! first hidden subsection figure](figs/fig_s1.jpg)

::: {.caption short-caption=""}
Figure 2.1.1: hidden! first hidden subsection figure
:::
::::

:::: {#fig:fig_h2_s .figure}
![Figure 2.1.2: hidden! second hidden subsection
figure](figs/fig_s2.jpg)

::: {.caption short-caption=""}
Figure 2.1.2: hidden! second hidden subsection figure
:::
::::

# 3 regular section

![Figure 3.1: first regular section
figure](figs/fig_s1.jpg){#fig:fig_r1}

![Figure 3.2: second regular section
figure](figs/fig_s2.jpg){#fig:fig_r2}

## 3.1 normal subsection {#normal-subsection hidden="n"}

![Figure 3.1.1: first normal subsection
figure](figs/fig_s1.jpg){#fig:fig_nss1}

![Figure 3.1.2: second normal subsection
figure](figs/fig_s2.jpg){#fig:fig_nss2}

## 3.2 hidden subsection {#hidden-subsection hidden="y"}

:::: {#fig:fig_ss1 .figure}
![Figure 3.2.1: hidden! first hidden subsection figure](figs/fig_s1.jpg)

::: {.caption short-caption=""}
Figure 3.2.1: hidden! first hidden subsection figure
:::
::::

:::: {#fig:fig_ss2 .figure}
![Figure 3.2.2: hidden! second hidden subsection
figure](figs/fig_s2.jpg)

::: {.caption short-caption=""}
Figure 3.2.2: hidden! second hidden subsection figure
:::
::::

![Figure 3.2.3: visible! overridden
visibility](figs/fig_s3.jpg){#fig:fig_ss3 hidden="n"}

:::: {#fig:subfigs .figure .subfigures}
![a](figs/fig_s2.jpg){#fig:fig_sss1}
![b](figs/fig_s2.jpg){#fig:fig_sss2}

::: caption
Figure 3.2.4: hidden! subfigures. a --- hidden! subfigure1, b ---
hidden! subfigure2
:::
::::

::: {#lst:listing .listing .haskell}
Listing 3.2.1: hidden!

``` haskell
main = putStrLn "Hello, World!"
```
:::

::: {#lst:listing2 .listing .haskell}
Listing 3.2.2: visible!

``` {.haskell hidden="n"}
main = putStrLn "Hello, World!"
```
:::

::: {#tbl:table1}
  foo   bar
  ----- -----
  1     10
  2     20
  3     30

  : Table 3.2.1: hidden! table
:::

::: {#tbl:table2 hidden="n"}
  foo   bar
  ----- -----
  1     10
  2     20
  3     30

  : Table 3.2.2: visible! table
:::

# 4 one more regular section

![Figure 4.1: first other regular section
figure](figs/fig_s1.jpg){#fig:fig_or1}

![Figure 4.2: second other regular section
figure](figs/fig_s2.jpg){#fig:fig_or2}

# List of Figures

::: {.list .list-of-fig}
1\. first figure\

2\. second figure\

3\. third figure\

3.1. first regular section figure\

3.2. second regular section figure\

3.1.1. first normal subsection figure\

3.1.2. second normal subsection figure\

3.2.3. visible! overridden visibility\

4.1. first other regular section figure\

4.2. second other regular section figure\
:::

# List of Listings

::: {.list .list-of-lst}
3.2.2. visible!\
:::

# List of Tables

::: {.list .list-of-tbl}
3.2.2. visible! table\
:::
