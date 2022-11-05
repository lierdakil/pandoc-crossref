---
lofItemTitle: "Figure&nbsp;"
lotItemTitle: "Title&nbsp;"
lolItemTemplate: "Listing&nbsp;$$i$$: $$t$$\\\n"
codeBlockCaptions: true
---

![1](fig1.png){#fig:1}

![2](fig2.png){#fig:2}

![3](fig3.png){#fig:3}

![4](fig4.png){#fig:4}

![5](fig5.png){#fig:5}

![6](fig6.png){#fig:6}

![7](fig7.png){#fig:7}

![8](fig8.png){#fig:8}

![9](fig9.png){#fig:9}

```haskell
main :: IO ()
main = putStrLn "Hello World!"
```
: Listing caption 1 {#lst:code1}

```haskell
main :: IO ()
main = putStrLn "Hello World!"
```

: Listing caption 2 {#lst:code2}

```{#lst:code3 .haskell}
main :: IO ()
main = putStrLn "Hello World!"
```
: Listing caption 3

```{#lst:code4 .haskell}
main :: IO ()
main = putStrLn "Hello World!"
```

: Listing caption 4

***

a   b   c
--- --- ---
1   2   3
4   5   6

: My table {#tbl:mytable}

| a | b |
|---|---|
| 1 | 2 |

: Table {#tbl:1}

\listoffigures

\listoftables

\listoflistings
