## 0.3.15.2

-   Fix single-row subfigures

    Subfigures in a single row weren't handled correctly since 0.3.15.0. See <https://github.com/lierdakil/pandoc-crossref/issues/381> for details.

## 0.3.15.1

-   Do not copy image attributes to implicit figure element

## 0.3.15.0

-   Update demos
-   Updates for pandoc 3.0 and bump version
-   Reshuffle and deduplicate block handling code

## 0.3.14.0

### Main changes

-   Add list-of-x item templates

    New template variables, `lofItemTemplate`, `lotItemTemplate`,
    `lolItemTemplate`, allowing to specify formatting for elements in lists-of
    items.

    Default values for templates also use variables `lofItemTitle`,
    `lotItemTitle`, `lolItemTitle`, `listItemTitleDelim`.

    Historically, list-of-x lists were formatted using markdown ordered lists when not using `chapters: true`. However, this is not enabled by default any more. You can restore the old behaviour by specifying the template using ordered list explicitly, e.g.

    ```yaml
    lofItemTemplate: |
        1. $$t$$
    ```

    Note that custom numbering (including chapter numbers) will be ignored by ordered lists, so it only works well for the simplest numbering schemes.

    Bullet lists are also now supported.

-   Fix cleveref latex preamble

    Cleveref now complains if it's loaded before hyperref, hence loading cleveref was wrapped into `AtEndPreamble`.

## 0.3.13.0

### Main changes

-   More flexible equation templates

    See [the documentation](https://lierdakil.github.io/pandoc-crossref/#equation-templates).

    **NOTICE**: Breaking change. If you're using `equationNumberTeX`, now
    `eqnIndexTemplate` gets applied to the index. Besides, the special handling
    of `qquad` is removed. TL;DR: if using `equationNumberTeX`, add `eqnIndexTemplate: $$i$$` to your metadata.

    Also note that the default behaviour is changed slightly, curly braces are added around the equation index. This shouldn't affect the rendered result, but if you need to restore the old behaviour, set `eqnInlineTemplate` as follows:

    ```yaml
    eqnInlineTemplate: $$e$$$$equationNumberTeX$$ $$i$$
    ```

-   Bump minimal pandoc version to 2.18

    Pandoc 2.18 changed some formatting, so tests are only compatible with
    pandoc 2.18, hence the version bump. In a pinch, you can build with older
    pandoc by using cabal's/stack's `--allow-newer` flag.

### Genreal Maintenance

-   Vendor (modified) roman-numerals library
-   Update tests for pandoc 5f0bfd41 (hseg, PR 348)
-   Complete purge of pandoc-citeproc references (gesh, PR #302)

## 0.3.12.2

-   Make section numbering more consistent with pandoc

    If the first heading in your document is level 2 or more, pandoc will assign
    index `0` to the implicit sections. Pandoc-crossref, on the other hand,
    assinged index `1` to these sections previously. This could lead to an
    off-by-one discrepancy between section and reference indices when using
    pandoc's `--number-sections`.

    This behaviour is now aligned with pandoc, i.e. missing top-level sections
    will get index of `0`.

    The old behaviour is kept when using pandoc-crossref's `numberSections`,
    for backwards compatibility and also because having references start with
    `0` is rather impractical to begin with.


## 0.3.12.1

-   Fix docx tableEqns

## 0.3.12.0

-   Switch from data-accessor to microlens

## 0.3.11.0

-   Add equationNumberTeX option
-   Add title variable to refIndexTemplate; make refIndexTemplate per-prefix
-   Update to pandoc 2.14
-   Docs updates for --citeproc

## 0.3.10.0

-   \[Fix\] Avoid expensive set difference, use filter
-   \[Tweak\] For div-wrapped listings merge attributes and classes

    Previously, classes and attributes on the wrapping div were ignored. Now, those are merged with classes and attributes on the listing.

-   \[Feat\] Add option to set label attributes to actual numbering used

    New option `setLabelAttribute` which sets `label` attribute on sections, figures, etc to actual number used for referencing. This can be useful for post-processing.

-   \[Fix\] Omit empty section\/item labels

    Objects (sections, figures) can have empty labels (i.e. no number). Previously, if those labels were referenced (particularly with chapters enabled) this would create references like `fig 1..2`. Now these labels are omitted. It can lead to duplicate numbers in references though.

-   \[Fix\] Chapter delimiter in section numbers should be chapDelim (was '.')

    `chapDelim` now observed in section numbers (when `numberSections` is enabled)

## 0.3.9.1

-   [CI] Bump pandoc version
-   [Stack] Bump versions in stack.yaml
-   [LaTeX][Tests] Update tests for subfigures
-   [LaTeX][Tests] Update tests for pandoc 2.11.4
-   [LaTeX] Remove footnotes from "short" subfigure caption (used for lof)
-   [LaTeX] An attempt at fixing footnotes inside subfigure env

Note that this is a fix-release, but it includes some rather drastic changes to
the way subfigures are handled in LaTeX output. Couldn't fix the issue
otherwise. See <https://github.com/lierdakil/pandoc-crossref/issues/292> for
context and some additional pointers.

## 0.3.9.0

### New features

-   Add header multilevel templates and secLevelLabels
-   Add lowercase roman custom numbering

### Fixes

-   Label precedence fix: label attr > secLevelLabels > xLabels

### Misc

-   Update LaTeX tests for pandoc 2.11.3.1
-   Update docs
-   Relax bound on optparse-applicative

## 0.3.8.4

-   [Fix] secLabels
-   [ManData] Generate table of contents in embedded html; filter out kramdown toc marker
-   [Docs] Add toc to gh-pages

## 0.3.8.3

-   [Fix] Pandoc 2.11.0.1 fixes the issue fixed in the previous release upstream.

    Unfortunately, "double fix" is no fix at all. This release checks for pandoc version and disables the fix on our side as needed.

## 0.3.8.2

-   [Fix] Escape LaTeX caption attribute when taken from Markdown

    Apparently, pandoc (up to 2.11) doesn't escape `caption` code block attribute for LaTeX when used with `--listings`. This may be intentional, but when caption is defined in pandoc-crossref using div or table-style caption syntax, this can lead to inconsistencies. Hence, caption defined in Markdown (as opposed to as string in the attribute) is now escaped when outputting to LaTeX with `--listings` enabled.

## 0.3.8.1

-   Fix parsing multiple inlines in block

## 0.3.8.0

-   [LaTeX] Move hypertarget inside equation environment

    Fixes spacing issues in LaTeX/pdf output

-   [DOCX] Center equation number vertically with tableEqns

## 0.3.7.0

-  Support for pandoc-2.10

## 0.3.6.4

-   Last version to support pandoc 2.9
-   Create hypertargets on equations in LaTeX output

## 0.3.6.3

-   Fixed format option order in pipe mode
-   Bump to GHC 8.8.3
-   CI tweaks

## 0.3.6.2

-   Force WS state evaluation to resolve duplicate id error inconsistency

    See [#246](https://github.com/lierdakil/pandoc-crossref/issues/246) for context

##  0.3.6.1

-   Fix `crossrefYaml` setting broken in v0.3.6.0

##  0.3.6.0

-   Updates for pandoc-2.9

##  0.3.5.0

-   Updates for pandoc 2.8

-   Pandoc < 2.8 no longer supported

-   Check pandoc version when running through pandoc and issue a warning
    when it doesn't match the one pandoc-crossref was built with.

-   Running pandoc-crossref as a UNIX pipe implicitly, i.e.
    `pandoc -t json text.md | pandoc-crossref | pandoc -f json`
    is no longer supported. Use the `--pipe` (short `-p` is also supported) option, e.g.
    `pandoc -t json text.md | pandoc-crossref --pipe | pandoc -f json`

-   Running pandoc-crossref as a UNIX pipe with specified output format, i.e.
    `pandoc -t json text.md | pandoc-crossref html | pandoc -f json`
    is deprecated. Please use the `--pipe` (short `-p` is also supported) option, e.g.
    `pandoc -t json text.md | pandoc-crossref --pipe html | pandoc -f json`

## 0.3.4.2

-   Fix eqLabels in documentation and default meta
-   Readme updates

## 0.3.4.1

-   Fix custom labels defined via list (and clarify docs)
-   Encoding-aware console output
-   Changes to CI
-   Cosmetic fixes
-   Documentation updates

## 0.3.4.0

-   Updates for pandoc 2.4 and GHC 8.6
-   Fixed link in documentation (Salim B)
-   Fixed demo.md link in README
-   Corrected minor mistakes in the readme. (Wandmalfarbe)

## 0.3.3.0

-   Hide flaky tests behind a flag
-   Update for Pandoc-2.3
-   Add git commit info to version output
-   Handle SoftBreak consistenlty (especailly after math)

## 0.3.2.1

-   Make compatible with pandoc-types 1.17.5
-   bump stack resolver

## 0.3.2.0

-   Pandoc 2.2; GHC 8.4

## 0.3.1.0

-   Update documentation
-   Tweak default secHeaderDelim
-   Test for secHeaderTemplate and indexed template variables
-   New template syntax: indexed variables
-   secHeaderTemplate
-   Escape underscores in codeblock captions when necessary
-   Travis set sudo: true
-   Allow newer in stack.yaml
-   Set license to GPL-2 (because hackage refuses to accept GPL now)

## 0.3.0.3

-   Fix for Pandoc's Monoid\/Semigroup change
-   Add some information on distribution support
-   Add changelog to distribution

## 0.3.0.2

-   Constrain pandoc to 2.1.\*, bump version
-   Set stack reslover to new lts
-   Remove ghc8.0 stack
-   Update tests for Pandoc 2.1

## 0.3.0.1

-   Add `-threaded` option to executable
-   Show more version information on `pandoc-crossref --version`
-   Add `pandoc-crossref --numeric-version`
-   GPL license text: v2 or later
-   Update tests for Pandoc 2.0.6
-   Note on Pandoc 2.0.6 wrt raw LaTeX list-of commands
-   Treat beamer as LaTeX format (which it is)

## 0.3.0.0

-   Remove man page from git
-   Generate manual on CI, add it to archive
-   Stack update
-   Fix contact information in Settings.hs (Masamichi Hosoda)
-   Fix link to LICENSE in README
-   Add contact info to GPL header; Add (c)s according to commit history
-   Use license specifier recognized as or-later; rename license file
-   Add GPL header comment to every source file
-   Remove BSD3.md (obsolete)
-   Update contributors
-   Use YAML meta block in man
-   Add datafile, custom embedding, fix html man
-   Get rid of (most) generated doc files
-   Update generated man
-   Fix templates in man
-   Fix demo.md link
-   Hack to stop github from rendering demo.md
-   Remove extraneous `-i` argument from pandoc invocation

## 0.3.0.0-beta3a

-   Disable travis cache
-   Fix type for optparse-applicative 0.14

## 0.3.0.0-beta3

-   Raise travis build time limit
-   Meta: error on unexpected field values
-   Fix demo.md test

## 0.3.0.0-beta2

-   Read settings file with pandocExtensions
-   Read MetaStrings as Markdown (no formatting)
-   Fixed demos
-   Moved demo from gh-pages
-   Fix option parsing wrt format
-   Added `nameInLink` docs
-   Flags to open manual; embed manual
-   Textual manual
-   Add command line options
-   Moved documentation to docs/

## 0.3.0.0-beta

-   Pandoc-2.0 support

## 0.2.7.0

-   Revamp ModifyMeta, add subfloat caption margin
-   Update README due to \#132
-   fix typo in README: "defualt" -\> "defualt" (Raphael Das Gupta)
-   fix typo in README: "cleferef" -\> "cleveref" (Raphael Das Gupta)
-   Integrative tests for code blocks
-   Allow codeBlock label specification in caption
-   Moved getRefLabel to Util.Util

## 0.2.6.0

-   pairDelim, lastDelim, refDelim
-   Drop support for old pandoc/ghc versions
-   Report undefined references on stderr
-   Set UTF-8 encoding for reading `pandoc-crossref.yaml` (\#123)
    (Masamichi Hosoda)
-   Fix a typo in README (Felix Yan)
-   Option for including prefix in reference link

## 0.2.5.0

-   Global settings file support
-   pandoc 1.19 support and binaries
-   Better naming for `stack.yaml`s

## 0.2.4.2

-   Trim down dependencies
-   Center subfigures in LaTeX output

## 0.2.4.1

-   Force caption style in docx output (\#87)

## 0.2.4.0

-   Index templates
-   Link cross-references
-   Enable https flag on main stack.yaml
-   Fix stack.yaml configs
-   Add pandoc-1.18 to tests; allow pandoc-1.18 in dep
-   Eschew geq
-   Update ghc8 resolver
-   Number sections
-   Fix base dependency to \>= 4.8
-   Support for `supressAuthor` in cleveref tex output
-   Treat cit. SuppressAuthor as 'suppress prefix'
-   Update stack resolver
-   Support citation prefixes (override default prefix)
-   Use lambda instead of `ap`
-   Simplify makeIndices, better missed index reports

## 0.2.3.0

-   More tests
-   Major code cleanup, simplification
-   Make tableEqns work for inline display math
-   Documentation update
-   Fix custom numbering of auto eqns/auto subfigs
-   Filter out empties from collected subfigure captions
-   Properly sort collected subfigure captions
-   Simplify default settings
-   subfigGrid -- allows to typeset subfigures in a table in non-LaTeX
    output (LaTeX can typeset subfigures by itself)
-   Fix codelisting caption in LaTeX output
-   tableEqns -- allows to typeset numbered display math with 100%-wide
    tables. Primarily useful in docx, epub.
-   Option to auto-label equations
-   Error on duplicate labels

## 0.2.2.0

-   Add ghc-8 to tests/builds
-   Ghc-8 support
-   Minor lint
-   Remove UnicodeSyntax
-   Update tests for pandoc 1.17.1
-   Remove obsolete CPP
-   Remove old yamls/tests
-   Bump stack resolver and extra-deps
-   \[README\] Structure variables list
-   Add licensing information to readme
-   xPrefixTemplate docs
-   Custom numbering readme

## 0.2.1.3

-   Bump nightly resolver
-   Use Pandoc to generate LaTeX labels
-   Rudimentary LaTeX tests
-   Fixing broken references in README (pull request \#68 from
    @wladston)

## 0.2.1.2

-   Remove figure label hack for pandoc-1.17

## 0.2.1.1

-   Only apply ux5f substitution to sections

## 0.2.1.0

-   Replace \_ in LaTeX references with ux5f
-   Allow data-default 0.6 (pull request \#67 from @felixonmars)
-   tableEqns option wraps numbered eqns into tables
-   Allow arbitrary formatting in subfigures
-   Update changelog

## 0.2.0.1

-   Add modules to test-suite in .cabal
-   Allow builds with pandoc 1.17
-   Update stack resolver & deps
-   Remove obsolete CPP language extensions

## 0.2.0.0

-   **NOTE** Only pandoc-1.16 supported since this release
-   README updates
-   Subfigures!
-   Use SYB for walking AST
-   Custom subfigure labels
-   Custom numbering schemes
-   Allow arbitrary internal vars in template
-   Extensive TemplateHaskell derivations

## 0.1.6.4

-   Fix \#53
-   Use data-accessor package
-   Update README.md

## 0.1.6.3

-   Source code organization
-   Removed redundant brackets
-   Fix listing captions for pandoc-1.16 and up
-   restrictions on yaml settings - closes \#45 (pull request \#46 from
    @scoavoux)

## 0.1.6.2

-   **NOTE**: GHC 7.4 is unsupported since this release. Should be still
    possible to build on it though with careful dependency management.
-   Merge pull request \#44 from lierdakil/pandoc-1.16
-   Link travis badge to travis page
-   Make Travis happy
-   Update CHANGELOG
-   Version bump
-   README: Grammar

## 0.1.6.1

-   Adjust package constraints to match Pandoc's
-   Pandoc 1.16 update
-   Added CHANGELOG.md
-   Update README.md
-   Add stack config file (\#39 @hadim)
-   add info about where it might be installed on Macs (\#40 from
    @msalganik)

## 0.1.6.0

-   Version bump
-   Update demo.md
-   Work with inline equations (\#42)
-   Remove invalid tests

## 0.1.5.6

-   Version bump: relax API
-   add info about where it might be installed on Macs
-   Add stack config file
-   Add other-extensions
-   Remove check.sh from Travis
-   More robust tests

## 0.1.5.5

-   Version bump
-   Keep image title

## 0.1.5.4

-   Version bump
-   Add note on `--number-sections` and LaTeX
-   section cref prefix

## 0.1.5.3

-   Version bump
-   Don't use TemplateHaskell
-   Fix div-wrapped table captions
-   Simplify TH code
-   Fix native test
-   Section reference labels
-   \[Travis\] Try master branch cache on fail

## 0.1.5.2

-   Prepare v0.1.5.2 release
-   Documentation and public interface cleanup
-   Travis haskell template with homebrew cache

## 0.1.5.1

-   Prepare v0.1.5.1 release
-   Return Data.Monoid import to test-suite

## 0.1.5.0

-   Merge branch 'master' of github.com:lierdakil/pandoc-crossref
-   Prepare 0.1.5.0 release
-   Cleanup
-   SettingsGen accepts ToMetaValue arguments
-   TH Setting generators
-   Move most code to library
-   Merge pull request \#34 from infotroph/infotroph-patch-1
-   Fix typo: fixPrefix-\>figPrefix
-   Merge pull request \#32 from bmesuere/master
-   move section labels outside of code block section

## 0.1.4.0

-   Version bump
-   Div-wrapping for better compatibility with other filters

## 0.1.3.0

-   Add chaptersDepth info to README
-   Version bump
-   Removed unused variable
-   autoSectionLabels impl., tests and more docs
-   Section references docs
-   Tests for section references
-   Merge branch 'master' into section-refs
-   Relax test pandoc constraints
-   Update demo\*.native
-   Update .gitignore
-   Update demo\*.native
-   Update .gitignore
-   Section references support
-   FIX: Split references to items from different chapters with comma
-   Configurable "chapters" option depth
-   Add chapters output test

## 0.1.2.4

-   Version bump
-   Fix LaTeX listing numbering with chapters
-   Fix lists for chapters-enabled cases

## 0.1.2.3

-   Version bump
-   Update demo.native
-   Fix demo.md
-   Don't output zeroeth chapter prefix
-   BIGFIX: start numeration over for each chapter
-   Don't increment chapter counter for unnumbered chapters
-   Disable Travis caching for now
-   Try caching Travis build
-   Try installing all cabal/ghc versions
-   Travis container-based infrastructure update

## 0.1.2.2

-   Bump version number
-   Relax constraints to include pandoc-1.15
-   Update README with info on LaTeX customization
-   BUGFIX: Add metasettings not only to list
-   Relax test constraints
-   More travis tests

## 0.1.2.1

-   Version bump
-   Check.sh .native update
-   Regression test for \#22
-   Citation groups should not be separated

## 0.1.2.0

-   Honor uppercase for cref output
-   Bump version
-   Update tests
-   Update README
-   Allow mixing different types of citations
-   Pull cref uppercase settings from metadata
-   Reference prefix capitalization initial
-   Require semicolon after lst

## 0.1.1.0

-   README typo
-   Add links to listings examples
-   Add pandoc 1.14 info to readme
-   Add listoflistings to README
-   Add listoflistings to demo.md
-   List of names in LaTeX, list of listings
-   Update README
-   Readme syntax update
-   Add new files to .cabal
-   Update README.md
-   Use command defs for LaTeX customization
-   Fix cabal warnings
-   Merge branch 'pandoc-1.14'
-   Demo.md update
-   Fix no LaTeX ref for some CodeBlocks
-   Don't fail on test
-   Use preprocessor to work with pandoc 1.13 and 1.14
-   Merge branch 'master' into pandoc-1.14
-   Update README
-   Demo.native update
-   Tests update
-   Exclude caption attribute from codeblock
-   CodeBlock reference title fixes
-   Merge branch 'listings'
-   Only change header-includes on latex export
-   Crefnames from meta, convert to LaTeX
-   isFormat accepts Maybe Format now
-   Support different prefix for multiple references
-   Set LaTeX float names from metadata
-   ModifyMeta initial
-   Cleveref should not have spaces after commas in list
-   Demo.md update
-   Main code for listings and code block captions
-   Fix demo.md link
-   Bump tag

## 0.1.0.2

-   Bump version
-   Fix spacing issue in LaTeX
-   Initial support for suffixes
-   Merge branch 'master' into pandoc-1.14
-   Simplify tests
-   Update source-repo tag

## 0.1.0.1

-   Merge branch 'master' of github.com:lierdakil/pandoc-crossref
-   Use nbsp between ref. label and number
-   Use nbsp between ref. label and number
-   Update demo.native to match recent changes
-   Add other-modules in tests
-   Enable cabal tests in Travis
-   More tests
-   Some tests, general cleanup, normalization
-   Merge branch 'master' into pandoc-1.14
-   Template code cleanup
-   Code cleanup
-   Enable GHC 7.10.1 test
-   Fix typo
-   Update travis badge
-   Travis badge update
-   Travis test update
-   Bump version
-   Update master code for pandoc-1.14
-   Merge branch 'master' into pandoc-1.14

## 0.1.0.0, origin/release-0.1

-   Update README
-   Include other-modules into cabal file
-   Include limits on all depends
-   Fix check to work with binary
-   Merge branch 'master' into release-0.1
-   Try finally fixing travis build
-   Use bash test
-   Fix test run
-   Remove GHC 7.10 from testsuite
-   Exec pandoc through caba;
-   Relax more
-   Relax requirements on base
-   Travis badge in README
-   Add rudimentary Travis tests
-   Merge branch 'master' into pandoc-1.14
-   Attempt at fixing GHC 7.10 compatibility
-   Add meta guards to settings automatically
-   Use Meta instead of Yaml and mappend default
-   Split program to modules
-   Cabal settings
-   IOError handling
-   Add README description of settings file
-   Add support for pandoc-crossref.yaml
-   Updates for pandoc-1.14
-   Fix latex table caption
-   Update demo.md with tableTemplate
-   Fix template default values
-   Caption templates
-   Update README.md
-   Update README.md
-   Update README on spacing rules
-   Keep reference tags with label
-   Accept latex with extensions as latex
-   Include chapter number in item numbers as well
-   Update README
-   Update test
-   Don't output reference prefix with cleveref
-   Initial support for --chapters
-   Handle empty lists
-   Better metadata customization
-   Update README
-   Lists headings
-   Update README.md
-   List of figures, list of tables
-   More code cleanup
-   More code cleanup; store titles
-   More code cleanup
-   Rudimentary test
-   Code cleanup, more lax spaces rules
-   Create BSD3.md
-   Update data-accessor copyrights etc
-   Add reference prefix in latex output
-   Update README
-   Fix README
-   README.md update
-   Title delimiter option
-   Support for tables
-   Create LICENSE.md
-   Initial commit
