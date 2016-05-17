## 0.2.1.3
* Bump nightly resolver
* Use Pandoc to generate LaTeX labels
* Rudimentary LaTeX tests
* Fixing broken references in README (pull request #68 from @wladston)

## 0.2.1.2
* Remove figure label hack for pandoc-1.17

## 0.2.1.1
* Only apply ux5f substitution to sections

## 0.2.1.0
* Replace _ in LaTeX references with ux5f
* Allow data-default 0.6 (pull request #67 from @felixonmars)
* tableEqns option wraps numbered eqns into tables
* Allow arbitrary formatting in subfigures
* Update changelog

## 0.2.0.1
* Add modules to test-suite in .cabal
* Allow builds with pandoc 1.17
* Update stack resolver & deps
* Remove obsolete CPP language extensions

## 0.2.0.0
* **NOTE** Only pandoc-1.16 supported since this release
* README updates
* Subfigures!
* Use SYB for walking AST
* Custom subfigure labels
* Custom numbering schemes
* Allow arbitrary internal vars in template
* Extensive TemplateHaskell derivations

## 0.1.6.4
* Fix #53
* Use data-accessor package
* Update README.md

## 0.1.6.3
* Source code organization
* Removed redundant brackets
* Fix listing captions for pandoc-1.16 and up
* restrictions on yaml settings - closes #45 (pull request #46 from @scoavoux)

## 0.1.6.2
* **NOTE**: GHC 7.4 is unsupported since this release. Should be still possible to build on it though with careful dependency management.
* Merge pull request #44 from lierdakil/pandoc-1.16
* Link travis badge to travis page
* Make Travis happy
* Update CHANGELOG
* Version bump
* README: Grammar

## 0.1.6.1
* Adjust package constraints to match Pandoc's
* Pandoc 1.16 update
* Added CHANGELOG.md
* Update README.md
* Add stack config file (#39  @hadim)
* add info about where it might be installed on Macs (#40 from @msalganik)

## 0.1.6.0
* Version bump
* Update demo.md
* Work with inline equations (#42)
* Remove invalid tests

## 0.1.5.6
* Version bump: relax API
* add info about where it might be installed on Macs
* Add stack config file
* Add other-extensions
* Remove check.sh from Travis
* More robust tests

## 0.1.5.5
* Version bump
* Keep image title

## 0.1.5.4
* Version bump
* Add note on `--number-sections` and LaTeX
* section cref prefix

## 0.1.5.3
* Version bump
* Don't use TemplateHaskell
* Fix div-wrapped table captions
* Simplify TH code
* Fix native test
* Section reference labels
* [Travis] Try master branch cache on fail

## 0.1.5.2
* Prepare v0.1.5.2 release
* Documentation and public interface cleanup
* Travis haskell template with homebrew cache

## 0.1.5.1
* Prepare v0.1.5.1 release
* Return Data.Monoid import to test-suite

## 0.1.5.0
* Merge branch 'master' of github.com:lierdakil/pandoc-crossref
* Prepare 0.1.5.0 release
* Cleanup
* SettingsGen accepts ToMetaValue arguments
* TH Setting generators
* Move most code to library
* Merge pull request #34 from infotroph/infotroph-patch-1
* Fix typo: fixPrefix->figPrefix
* Merge pull request #32 from bmesuere/master
* move section labels outside of code block section

## 0.1.4.0
* Version bump
* Div-wrapping for better compatibility with other filters

## 0.1.3.0
* Add chaptersDepth info to README
* Version bump
* Removed unused variable
* autoSectionLabels impl., tests and more docs
* Section references docs
* Tests for section references
* Merge branch 'master' into section-refs
* Relax test pandoc constraints
* Update demo*.native
* Update .gitignore
* Update demo*.native
* Update .gitignore
* Section references support
* FIX: Split references to items from different chapters with comma
* Configurable "chapters" option depth
* Add chapters output test

## 0.1.2.4
* Version bump
* Fix LaTeX listing numbering with chapters
* Fix lists for chapters-enabled cases

## 0.1.2.3
* Version bump
* Update demo.native
* Fix demo.md
* Don't output zeroeth chapter prefix
* BIGFIX: start numeration over for each chapter
* Don't increment chapter counter for unnumbered chapters
* Disable Travis caching for now
* Try caching Travis build
* Try installing all cabal/ghc versions
* Travis container-based infrastructure update

## 0.1.2.2
* Bump version number
* Relax constraints to include pandoc-1.15
* Update README with info on LaTeX customization
* BUGFIX: Add metasettings not only to list
* Relax test constraints
* More travis tests

## 0.1.2.1
* Version bump
* Check.sh .native update
* Regression test for #22
* Citation groups should not be separated

## 0.1.2.0
* Honor uppercase for cref output
* Bump version
* Update tests
* Update README
* Allow mixing different types of citations
* Pull cref uppercase settings from metadata
* Reference prefix capitalization initial
* Require semicolon after lst

## 0.1.1.0
* README typo
* Add links to listings examples
* Add pandoc 1.14 info to readme
* Add listoflistings to README
* Add listoflistings to demo.md
* List of names in LaTeX, list of listings
* Update README
* Readme syntax update
* Add new files to .cabal
* Update README.md
* Use command defs for LaTeX customization
* Fix cabal warnings
* Merge branch 'pandoc-1.14'
* Demo.md update
* Fix no LaTeX ref for some CodeBlocks
* Don't fail on test
* Use preprocessor to work with pandoc 1.13 and 1.14
* Merge branch 'master' into pandoc-1.14
* Update README
* Demo.native update
* Tests update
* Exclude caption attribute from codeblock
* CodeBlock reference title fixes
* Merge branch 'listings'
* Only change header-includes on latex export
* Crefnames from meta, convert to LaTeX
* isFormat accepts Maybe Format now
* Support different prefix for multiple references
* Set LaTeX float names from metadata
* ModifyMeta initial
* Cleveref should not have spaces after commas in list
* Demo.md update
* Main code for listings and code block captions
* Fix demo.md link
* Bump tag

## 0.1.0.2
* Bump version
* Fix spacing issue in LaTeX
* Initial support for suffixes
* Merge branch 'master' into pandoc-1.14
* Simplify tests
* Update source-repo tag

## 0.1.0.1
* Merge branch 'master' of github.com:lierdakil/pandoc-crossref
* Use nbsp between ref. label and number
* Use nbsp between ref. label and number
* Update demo.native to match recent changes
* Add other-modules in tests
* Enable cabal tests in Travis
* More tests
* Some tests, general cleanup, normalization
* Merge branch 'master' into pandoc-1.14
* Template code cleanup
* Code cleanup
* Enable GHC 7.10.1 test
* Fix typo
* Update travis badge
* Travis badge update
* Travis test update
* Bump version
* Update master code for pandoc-1.14
* Merge branch 'master' into pandoc-1.14

## 0.1.0.0, origin/release-0.1
* Update README
* Include other-modules into cabal file
* Include limits on all depends
* Fix check to work with binary
* Merge branch 'master' into release-0.1
* Try finally fixing travis build
* Use bash test
* Fix test run
* Remove GHC 7.10 from testsuite
* Exec pandoc through caba;
* Relax more
* Relax requirements on base
* Travis badge in README
* Add rudimentary Travis tests
* Merge branch 'master' into pandoc-1.14
* Attempt at fixing GHC 7.10 compatibility
* Add meta guards to settings automatically
* Use Meta instead of Yaml and mappend default
* Split program to modules
* Cabal settings
* IOError handling
* Add README description of settings file
* Add support for pandoc-crossref.yaml
* Updates for pandoc-1.14
* Fix latex table caption
* Update demo.md with tableTemplate
* Fix template default values
* Caption templates
* Update README.md
* Update README.md
* Update README on spacing rules
* Keep reference tags with label
* Accept latex with extensions as latex
* Include chapter number in item numbers as well
* Update README
* Update test
* Don't output reference prefix with cleveref
* Initial support for --chapters
* Handle empty lists
* Better metadata customization
* Update README
* Lists headings
* Update README.md
* List of figures, list of tables
* More code cleanup
* More code cleanup; store titles
* More code cleanup
* Rudimentary test
* Code cleanup, more lax spaces rules
* Create BSD3.md
* Update data-accessor copyrights etc
* Add reference prefix in latex output
* Update README
* Fix README
* README.md update
* Title delimiter option
* Support for tables
* Create LICENSE.md
* Initial commit
