![Image 1](img.png){#fig:1 ref="img." ref="imgs." title="Test1"}

![Image 2](img.png){#fig:2 ref="img." ref="imgs." title="Test2"}

![Image 3](img.png){#fig:3}

This should have custom prefix: @fig:1

This should have custom prefix: @fig:2

This should have normal prefix: @fig:3

This should have custom prefix: [@fig:1; @fig:2]

This should have custom prefix: [@fig:1; @fig:2; @fig:3]

This should have normal prefix: [@fig:3; @fig:1; @fig:2]

This should have custom prefix (capitalized): @Fig:1

This should have custom prefix (capitalized): @Fig:2

This should have normal prefix (capitalized): @Fig:3

This should have custom prefix (capitalized): [@Fig:1; @Fig:2]

This should have custom prefix (capitalized): [@Fig:1; @Fig:2; @Fig:3]

This should have normal prefix (capitalized): [@Fig:3; @Fig:1; @Fig:2]
