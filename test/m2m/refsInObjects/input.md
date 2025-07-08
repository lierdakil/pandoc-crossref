---
codeBlockCaptions: true
...

# @sec:hdr {#sec:hdr}

![@fig:fig](foo.png){#fig:fig}

| | | |
|-|-|-|
| | | |

: @tbl:table @fig:subfigures @fig:subfig1 @fig:subfig2 @lst:lst {#tbl:table}

<!---->

```{#lst:lst .cpp}
foo
```

: Listing @tbl:table @fig:subfigures @fig:subfig1 @fig:subfig2 @lst:lst

<div id="fig:subfigures">
  ![@tbl:table @fig:subfigures @fig:subfig1 @fig:subfig2 @lst:lst](fig1.png){#fig:subfig1}
  ![@tbl:table @fig:subfigures @fig:subfig1 @fig:subfig2 @lst:lst](fig2.png){#fig:subfig2}
  ![3](fig3.png)

  Caption @tbl:table @fig:subfigures @fig:subfig1 @fig:subfig2 @lst:lst
</div>
