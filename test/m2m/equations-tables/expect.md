This is a test file with some referenced equations, line $$ this $$

Some equations might be inside of text, $$ for example $$ this one.

Some equations might be on start of paragraphs:

$$ start $$ of paragraph.

Other might be on separate paragraphs of their own:

$$ separate $$

Some of those can be labelled:

This is a test file with some referenced equations, line

::: {#eq:0}
  ---------------------------------------------------------------- ---------
                             $$ this $$                              $$(1)$$
  ---------------------------------------------------------------- ---------
:::

Some equations might be inside of text,

::: {#eq:1}
  ---------------------------------------------------------------- ---------
                         $$ for example $$                           $$(2)$$
  ---------------------------------------------------------------- ---------
:::

this one.

Some equations might be on start of paragraphs:

::: {#eq:2}
  ---------------------------------------------------------------- ---------
                            $$ start $$                              $$(3)$$
  ---------------------------------------------------------------- ---------
:::

of paragraph.

Other might be on separate paragraphs of their own:

::: {#eq:3}
  ---------------------------------------------------------------- ---------
                           $$ separate $$                            $$(4)$$
  ---------------------------------------------------------------- ---------
:::

Then they can be referenced:

Individually eq. 1, eq. 2, eq. 3, eq. 4

Or in groups eqns. 1, 2, 4

Groups will be compacted eqns. 1-4
