This is a test file with some referenced equations, line $$ this $$

Some equations might be inside of text, $$ for example $$ this one.

Some equations might be on start of paragraphs:

$$ start $$ of paragraph.

Other might be on separate paragraphs of their own:

$$ separate $$

Some of those can be labelled:

This is a test file with some referenced equations, line

<div id="eq:0">

  ---------------------------------------------------------------- ---------
                             $$ this $$                              $$(1)$$
  ---------------------------------------------------------------- ---------

</div>

Some equations might be inside of text,

<div id="eq:1">

  ---------------------------------------------------------------- ---------
                         $$ for example $$                           $$(2)$$
  ---------------------------------------------------------------- ---------

</div>

this one.

Some equations might be on start of paragraphs:

<div id="eq:2">

  ---------------------------------------------------------------- ---------
                            $$ start $$                              $$(3)$$
  ---------------------------------------------------------------- ---------

</div>

of paragraph.

Other might be on separate paragraphs of their own:

<div id="eq:3">

  ---------------------------------------------------------------- ---------
                           $$ separate $$                            $$(4)$$
  ---------------------------------------------------------------- ---------

</div>

Then they can be referenced:

Individually eq. 1, eq. 2, eq. 3, eq. 4

Or in groups eqns. 1, 2, 4

Groups will be compacted eqns. 1-4