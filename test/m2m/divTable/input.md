On a table in a crossref div, attributes remain untouched. It's a backwards compatibility thing.

:::{#tbl:table1}
foo   bar
----- -----
1     10
2     20
3     30

: table {.foo bar="baz"}
:::

However, if a table in a crossref div has id set, then its id takes precedence in LaTeX.

:::{#tbl:table_with_id}
foo   bar
----- -----
1     10
2     20
3     30

: table {#quux}
:::

Even if attributes are empty

:::{#tbl:table2}
foo   bar
----- -----
1     10
2     20
3     30

: table (no attributes)
:::

But tables without a div don't get wrapped since Pandoc 3.8.2:

foo   bar
----- -----
1     10
2     20
3     30

: table (crossref id) {#tbl:table3}
