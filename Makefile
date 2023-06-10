pin:
	nix build .#static
	cachix pin pandoc-crossref $$(git describe --tags) $$(nix eval --raw .#static) -a bin/pandoc-crossref --keep-revisions 1
