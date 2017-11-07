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

Individually eq. [1](#eq:0), eq. [2](#eq:1), eq. [3](#eq:2),
eq. [4](#eq:3)

Or in groups eqns. [1](#eq:0), [2](#eq:1), [4](#eq:3)

Groups will be compacted eqns. [1](#eq:0)-[4](#eq:3)

Unknown references will print labels eqns. **¿eq:none?**, [1](#eq:0),
[3](#eq:2), [4](#eq:3)

Reference prefix will override default prefix Equation [1](#eq:0),
eqns. [3](#eq:2), [4](#eq:3)

References with `-` prepended won't have prefix at all: [1](#eq:0),
[2](#eq:1), eqns. [3](#eq:2), [4](#eq:3)

References with suffix will have suffix printed after index
(configurable): eqns. [1](#eq:0), [2 suffix](#eq:1), [3](#eq:2)
