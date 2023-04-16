.cask: Cask
	cask install
	touch .cask

test:
	@deno run --allow-write --allow-run test.ts

build:
	cask emacs --batch --load jieba.el --eval '(jieba--dyn-build)'
