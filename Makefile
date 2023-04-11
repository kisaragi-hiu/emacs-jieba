build:
	cargo build
	-mv target/debug/libemacs_jieba.so target/debug/jieba-dyn.so

test: build
	emacs test.el

test-require-directly:
	cask install
	cask emacs --batch tests/require-directly.el
