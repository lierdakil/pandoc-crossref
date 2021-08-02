This is a test file with some referenced equations, line $$ this $$

Some equations might be inside of text, $$ for example $$ this one.

Some equations might be on start of paragraphs:

$$ start $$ of paragraph.

Other might be on separate paragraphs of their own:

$$ separate $$

Some of those can be labelled:

This is a test file with some referenced equations, line
[$$ this \qquad(I)$$]{#eqn:0}

Some equations might be inside of text,
[$$ for example \qquad(II)$$]{#eqn:1} this one.

Some equations might be on start of paragraphs:

[$$ start \qquad(III)$$]{#eqn:2} of paragraph.

Other might be on separate paragraphs of their own:

[$$ separate \qquad(IV)$$]{#eqn:3}

Then they can be referenced:

Individually eq. I, eq. II, eq. III, eq. IV

Or in groups eqns. I, II, IV

Groups will be compacted eqns. I-IV

Unknown references will print labels eqns. **¿eqn:none?**, I, III, IV

Reference prefix will override default prefix Equation I, eqns. III, IV

References with `-` prepended won't have prefix at all: I, II,
eqns. III, IV

References with suffix will have suffix printed after index
(configurable): eqns. I, II suffix, III
