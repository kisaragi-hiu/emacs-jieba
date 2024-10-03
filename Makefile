.cask: Cask
	cask install
	touch .cask

test-require:
	@deno run --allow-write --allow-run --allow-env test.ts

test-library: build
	@deno run --allow-write --allow-run --allow-env test.ts tests/test-jieba.el

test: .cask
	make test-require
	make test-library

# We need to add current path to load-path
# because I assume jieba.el's location would be in the load-path and therefore a
# good place for the dynamic library
build:
	cask emacs --batch \
		-L . \
		--eval "(setq jieba-dyn-get-method 'compile)" \
		--load jieba.el
