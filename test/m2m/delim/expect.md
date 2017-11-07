This is a test file with some referenced equations, line $$ this $$

Some equations might be inside of text, $$ for example $$ this one.

Some equations might be on start of paragraphs:

$$ start $$ of paragraph.

Other might be on separate paragraphs of their own:

$$ separate $$

Some of those can be labelled:

This is a test file with some referenced equations, line
[$$ this \qquad(1)$$]{#eq:0}

Some equations might be inside of text,
[$$ for example \qquad(2)$$]{#eq:1} this one.

Some equations might be on start of paragraphs:

[$$ start \qquad(3)$$]{#eq:2} of paragraph.

Other might be on separate paragraphs of their own:

[$$ separate \qquad(4)$$]{#eq:3}

Then they can be referenced:

Individually eq. 1, eq. 2, eq. 3, eq. 4

Or in groups eqns. 1ref2last4

Groups will be compacted eqns. 1range4

Unknown references will print labels eqns. **¿eq:none?**ref1ref3last4

Reference prefix will override default prefix Equation 1, eqns. 3pair4

References with `-` prepended won't have prefix at all: 1pair2,
eqns. 3pair4

References with suffix will have suffix printed after index
(configurable): eqns. 1ref2 suffixlast3
