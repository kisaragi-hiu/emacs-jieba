# emacs-jieba

Emacs Lisp bindings for [jieba-rs][jieba-rs].

## Status

It works, but only the default dictionary is available.

If the Rust toolchain (including Cargo) is available, the library will be built automatically.

I'm still figuring out how to build and release binaries for multiple platforms; auto download also isn't here yet.

## Thanks

- [emacs-module-rs](https://github.com/ubolonton/emacs-module-rs), which provides binding from Rust to the Emacs dynamic modules support
- [@node-rs/jieba](https://github.com/napi-rs/node-rs/tree/main/packages/jieba), whose code is the basis for this package
- [jieba-rs][jieba-rs], the binding, and [Jieba](https://github.com/fxsjy/jieba), the original

[jieba-rs]: https://github.com/messense/jieba-rs

## Prior Art

Jieba.el kind of counts, although it's more about integrating with [nodejieba](https://github.com/yanyiwu/nodejieba) in order to get word-wise commands to understand Mandarin words.

## Roadmap

- [ ] Support `load_dict`, `with_dict` (perhaps by reading a file instead of reading a string passed from Emacs)
- [ ] Support [TextRank](https://docs.rs/jieba-rs/latest/jieba_rs/struct.TextRank.html)
- [ ] Support suggest_freq
- [ ] Write docs

## Parts of speech

The “parts of speech” (or “tag”) argument is a string specifier. The full list can be seen in [Jieba's README](https://github.com/fxsjy/jieba).

| 標籤 | 含義     | 標籤 | 含義     | 標籤 | 含義     | 標籤 | 含義     |
|------|----------|------|----------|------|----------|------|----------|
| n    | 普通名詞 | f    | 方位名詞 | s    | 處所名詞 | t    | 時間     |
| nr   | 人名     | ns   | 地名     | nt   | 機構名   | nw   | 作品名   |
| nz   | 其他專名 | v    | 普通動詞 | vd   | 動副詞   | vn   | 名動詞   |
| a    | 形容詞   | ad   | 副形詞   | an   | 名形詞   | d    | 副詞     |
| m    | 數量詞   | q    | 量詞     | r    | 代詞     | p    | 介詞     |
| c    | 連詞     | u    | 助詞     | xc   | 其他虛詞 | w    | 標點符號 |
| PER  | 人名     | LOC  | 地名     | ORG  | 機構名   | TIME | 時間     |
