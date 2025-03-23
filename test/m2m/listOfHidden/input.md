---
chapters: true
chaptersDepth: 1000
numberSections: true
codeBlockCaptions: true
---

# main section {-}

blah blah blah [@fig:fig1; @fig:fig_s1]

![first figure](figs/fig1.jpg){#fig:fig1}

furthermore, etc., ([@fig:fig2; @fig:fig_s2])

![second figure](figs/fig2.jpg){#fig:fig2}

and finally ([@fig:fig3])

![third figure](figs/fig3.jpg){#fig:fig3}

# hidden supplemental section {label="S"}

![hidden! first supp figure](figs/fig_s1.jpg){#fig:fig_s1 hidden=y}

![hidden! second supp figure](figs/fig_s2.jpg){#fig:fig_s2 hidden=y}

# hidden section {hidden=y}

![hidden! first hidden figure](figs/fig_s1.jpg){#fig:fig_h1}

![hidden! second hidden figure](figs/fig_s2.jpg){#fig:fig_h2}

## subsection of a hidden section

![hidden! first hidden subsection figure](figs/fig_s1.jpg){#fig:fig_h1_s}

![hidden! second hidden subsection figure](figs/fig_s2.jpg){#fig:fig_h2_s}

# regular section

![first regular section figure](figs/fig_s1.jpg){#fig:fig_r1}

![second regular section figure](figs/fig_s2.jpg){#fig:fig_r2}

## normal subsection {hidden=n}

![first normal subsection figure](figs/fig_s1.jpg){#fig:fig_nss1}

![second normal subsection figure](figs/fig_s2.jpg){#fig:fig_nss2}

## hidden subsection {hidden=y}

![hidden! first hidden subsection figure](figs/fig_s1.jpg){#fig:fig_ss1}

![hidden! second hidden subsection figure](figs/fig_s2.jpg){#fig:fig_ss2}

![visible! overridden visibility](figs/fig_s3.jpg){#fig:fig_ss3 hidden=n}

:::{#fig:subfigs}
![hidden! subfigure1](figs/fig_s2.jpg){#fig:fig_sss1}
![hidden! subfigure2](figs/fig_s2.jpg){#fig:fig_sss2}

hidden! subfigures
:::

```{#lst:listing .haskell}
main = putStrLn "Hello, World!"
```
: hidden!

<!---->

```{#lst:listing2 .haskell hidden=n}
main = putStrLn "Hello, World!"
```
: visible!

<!---->

foo bar
--- ---
1   10
2   20
3   30

: hidden! table {#tbl:table1}

:::{#tbl:table2 hidden=n}
foo bar
--- ---
1   10
2   20
3   30

: visible! table
:::

# one more regular section

![first other regular section figure](figs/fig_s1.jpg){#fig:fig_or1}

![second other regular section figure](figs/fig_s2.jpg){#fig:fig_or2}

<!---->

\listoffigures

<!---->

\listoflistings

<!---->

\listoftables
