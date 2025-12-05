.PHONY: build pin push test update

export NIX_CONFIG := extra-experimental-features = nix-command flakes

build:
	nix build . .#static .#pandoc $(NIX_EXTRA_OPTS)

pin:
	nix build .#static $(NIX_EXTRA_OPTS)
	cachix pin pandoc-crossref $$(git describe --tags) $$(nix eval --raw .#static $(NIX_EXTRA_OPTS)) -a bin/pandoc-crossref --keep-revisions 1
	cachix pin pandoc-crossref $$(git describe --tags)-with-pandoc $$(nix eval --raw .#pandoc-with-crossref $(NIX_EXTRA_OPTS)) --keep-revisions 1

push:
	nix build . .#static .#pandoc .#pandoc-with-crossref $(NIX_EXTRA_OPTS) --json \
		| jq -r '.[].outputs | to_entries[].value' \
		| cachix push pandoc-crossref

test:
	nix run .#test $(NIX_EXTRA_OPTS) && nix run .#test-integrative $(NIX_EXTRA_OPTS)

regen-test-fixtures:
	nix develop --command bash -c './mkcheck.sh && ./mkinttest.sh'

cabal.project.freeze: .github/workflows/haskell.yml
	rm cabal.project.freeze || true
	cabal freeze \
		--with-compiler=ghc-$$(yq -r '.jobs.build.strategy.matrix.ghcver[0]' .github/workflows/haskell.yml) \
		--constraint pandoc==$$(yq -r '.env.PANDOC_VERSION' .github/workflows/haskell.yml)
	sed -ri '/ *\S+ [^=]/ s/ *[+-]pkg-config//' cabal.project.freeze
	sed -ri '/pandoc-crossref/ d' cabal.project.freeze

stack.yaml: cabal.project.freeze stack.template.yaml .github/workflows/haskell.yml
	echo "# THIS FILE IS GENERATED, DO NOT EDIT DIRECTLY" > stack.yaml
	sed 's/\$$ghcver\$$/'"$$(yq -r '.jobs.build.strategy.matrix.ghcver[0]' .github/workflows/haskell.yml)"'/g' \
		stack.template.yaml >> stack.yaml
	grep -Ev 'any\.(ghc-boot-th|ghc-prim|rts|base) ' cabal.project.freeze \
		| sed -rn 's/^\s*any.([^ ]*) ==([^, ]*)([^,]*),?$$/- \1-\2/p' \
		>> stack.yaml

flake.lock: .github/workflows/haskell.yml
	nix \
		flake update

stack.yaml.lock: .github/workflows/haskell.yml stack.yaml
	# need this to update stack.yaml.lock, feel free to kill after that
	stack build --no-system-ghc --no-install-ghc --no-nix || true

pandoc-crossref.cabal: package.yaml
	# just use stack to generate the cabalfile
	stack build --no-system-ghc --no-install-ghc --no-nix || true

update: stack.yaml flake.lock stack.yaml.lock

upload:
	cabal sdist
	cabal upload --publish dist-newstyle/sdist/$$(yq -r '.name + "-" + .version' package.yaml).tar.gz

upload-docs:
	cabal haddock --haddock-for-hackage
	cabal upload --publish -d dist-newstyle/$$(yq -r '.name + "-" + .version' package.yaml)-docs.tar.gz
