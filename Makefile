test: build
	emacs test.el

test-require-directly:
	cask install
	cask emacs --batch tests/require-directly.el

build:
	cask emacs --batch --load jieba.el --eval '(jieba--dyn-build)'
