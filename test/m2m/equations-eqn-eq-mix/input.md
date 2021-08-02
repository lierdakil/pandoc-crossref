This is a test file with some referenced equations, line $$ this $$

Some equations might be inside of text, $$ for example $$ this one.

Some equations might be on start of paragraphs:

$$ start $$ of paragraph.

Other might be on separate paragraphs of their own:

$$ separate $$

Some of those can be labelled:

This is a test file with some referenced equations, line $$ this $${#eqn:0}

Some equations might be inside of text, $$ for example $${#eq:0} this one.

Some equations might be on start of paragraphs:

$$ start $${#eqn:2} of paragraph.

Other might be on separate paragraphs of their own:

$$ separate $${#eqn:3}

Then they can be referenced:

Individually @eqn:0, @eq:0, @eqn:2, @eqn:3

Or in groups [@eqn:0; @eq:0; @eqn:3]

Groups will be compacted [@eqn:0; @eq:0; @eqn:3; @eqn:2]

Unknown references will print labels [@eqn:0; @eqn:none; @eqn:3; @eqn:2]

Reference prefix will override default prefix [Equation @eqn:0; @eqn:3; @eqn:2]

References with `-` prepended won't have prefix at all:
[-@eqn:0; -@eq:0; @eqn:2; @eqn:3]

References with suffix will have suffix printed after index (configurable):
[@eqn:0; @eq:0 suffix; @eqn:2]
