build:
	cargo build
	-mv target/debug/libemacs_jieba.so target/debug/jieba.so

test: build
	emacs test.el
