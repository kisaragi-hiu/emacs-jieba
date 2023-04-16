test:
	cask install
	cask emacs --batch tests/require-directly.el
	cask emacs --batch tests/other.el

build:
	cask emacs --batch --load jieba.el --eval '(jieba--dyn-build)'
