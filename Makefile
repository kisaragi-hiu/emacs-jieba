.cask: Cask
	cask install

test:
	cask install
	cask emacs --batch -l tests/other.el
	cask emacs --batch -l tests/require-directly.el

build:
	cask emacs --batch --load jieba.el --eval '(jieba--dyn-build)'
