.PHONY: build pin push test update

build:
	nix build . .#static .#pandoc $(NIX_EXTRA_OPTS)

pin:
	nix build .#static $(NIX_EXTRA_OPTS)
	cachix pin pandoc-crossref $$(git describe --tags) $$(nix eval --raw .#static $(NIX_EXTRA_OPTS)) -a bin/pandoc-crossref --keep-revisions 1

push:
	nix build . .#static .#pandoc $(NIX_EXTRA_OPTS) --json \
		| jq -r '.[].outputs | to_entries[].value' \
		| cachix push pandoc-crossref

test:
	nix run .#test $(NIX_EXTRA_OPTS) && nix run .#test-integrative $(NIX_EXTRA_OPTS)

regen-test-fixtures:
	nix develop --command bash -c './mkcheck.sh && ./mkinttest.sh'

cabal.project.freeze: .github/workflows/haskell.yml
	rm cabal.project.freeze || true
	cabal freeze --constraint pandoc==$$(yq '.env.PANDOC_VERSION' .github/workflows/haskell.yml)

stack.yaml: cabal.project.freeze stack.template.yaml
	echo "# THIS FILE IS GENERATED, DO NOT EDIT DIRECTLY" > stack.yaml
	cat stack.template.yaml >> stack.yaml
	grep -Ev 'any\.(ghc-boot-th|ghc-prim|rts|base) ' cabal.project.freeze \
		| sed -rn 's/^\s*any.(.*) ==([^,]*),?$$/- \1-\2/p' \
		>> stack.yaml

flake.lock: .github/workflows/haskell.yml
	nix flake update

stack.yaml.lock: .github/workflows/haskell.yml stack.yaml
	# need this to update stack.yaml.lock, feel free to kill after that
	stack build --no-system-ghc --no-install-ghc || true

update: stack.yaml flake.lock stack.yaml.lock
