.cask: Cask
	cask install
	touch .cask

test-require:
	@deno run --allow-write --allow-run test.ts

test-library:
	@deno run --allow-write --allow-run test.ts tests/test-jieba.el

test: test-require test-library

build:
	cask emacs --batch --load jieba.el --eval '(jieba--dyn-build)'
