This is a test file with some referenced equations, line $$ this $$

Some equations might be inside of text, $$ for example $$ this one.

Some equations might be on start of paragraphs:

$$ start $$ of paragraph.

Other might be on separate paragraphs of their own:

$$ separate $$

Some of those can be labelled:

This is a test file with some referenced equations, line <span
id="eq:0">$$ this \qquad(1)$$</span>

Some equations might be inside of text, <span
id="eq:1">$$ for example \qquad(2)$$</span> this one.

Some equations might be on start of paragraphs:

<span id="eq:2">$$ start \qquad(3)$$</span> of paragraph.

Other might be on separate paragraphs of their own:

<span id="eq:3">$$ separate \qquad(4)$$</span>

Then they can be referenced:

Individually eq. 1, eq. 2, eq. 3, eq. 4

Or in groups eqns. 1, 2, 4

Groups will be compacted eqns. 1-4

Unknown references will print labels eqns. **¿eq:none?**, 1, 3, 4

Reference prefix will override default prefix Equation 1, eqns. 3, 4

References with `-` prepended won't have prefix at all: 1, 2, eqns. 3, 4

References with suffix will have suffix printed after index
(configurable): eqns. 1, 2 suffix, 3
