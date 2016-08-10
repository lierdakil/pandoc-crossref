---
tableEqns: true
...

This is a test file with some referenced equations, line $$ this $$

Some equations might be inside of text, $$ for example $$ this one.

Some equations might be on start of paragraphs:

$$ start $$ of paragraph.

Other might be on separate paragraphs of their own:

$$ separate $$

Some of those can be labelled:

This is a test file with some referenced equations, line $$ this $${#eq:0}

Some equations might be inside of text, $$ for example $${#eq:1} this one.

Some equations might be on start of paragraphs:

$$ start $${#eq:2} of paragraph.

Other might be on separate paragraphs of their own:

$$ separate $${#eq:3}

Then they can be referenced:

Individually @eq:0, @eq:1, @eq:2, @eq:3

Or in groups [@eq:0; @eq:1; @eq:3]

Groups will be compacted [@eq:0; @eq:1; @eq:3; @eq:2]
