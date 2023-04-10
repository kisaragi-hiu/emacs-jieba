use emacs::{defun, Env, IntoLisp, Result, Value, Vector};
use jieba_rs::{Jieba, KeywordExtract, TFIDF};
use once_cell::sync::OnceCell;
use std::str;

emacs::plugin_is_GPL_compatible!();

static JIEBA: OnceCell<Jieba> = OnceCell::new();
static TFIDF_INSTANCE: OnceCell<TFIDF> = OnceCell::new();

// From emacs-tree-sitter
// https://github.com/emacs-tree-sitter/elisp-tree-sitter/blob/master/core/src/query.rs#L13
fn vec_to_vector<'e, T: IntoLisp<'e>>(env: &'e Env, vec: Vec<T>) -> Result<Vector<'e>> {
    let vector = env.make_vector(vec.len(), ())?;
    for (i, v) in vec.into_iter().enumerate() {
        vector.set(i, v)?;
    }
    Ok(vector)
}

#[emacs::module(name = "jieba")]
fn init(_env: &Env) -> Result<()> {
    Ok(())
}

/// Initialize Jieba, or do nothing if it is already initialized.
#[defun]
fn load() -> Result<()> {
    let _ = JIEBA.get_or_init(Jieba::new);
    Ok(())
}

/// Cut SENTENCE into an array of tokens.
#[defun]
fn cut<'e>(env: &'e Env, sentence: String, hmm: Option<Value>) -> Result<Vector<'e>> {
    let jieba = JIEBA.get_or_init(Jieba::new);
    let cutted = jieba.cut(sentence.as_str(), hmm.is_some());
    vec_to_vector(env, cutted)
}

#[defun]
fn cut_all(env: &Env, sentence: String) -> Result<Vector> {
    let jieba = JIEBA.get_or_init(Jieba::new);
    let cutted = jieba.cut_all(sentence.as_str());
    vec_to_vector(env, cutted)
}

#[defun]
fn cut_for_search<'e>(env: &'e Env, sentence: String, hmm: Option<Value>) -> Result<Vector<'e>> {
    let jieba = JIEBA.get_or_init(Jieba::new);
    let cutted = jieba.cut_for_search(sentence.as_str(), hmm.is_some());
    vec_to_vector(env, cutted)
}

// struct TaggedWord {
//     pub tag: String,
//     pub word: String,
// }

// #[defun]
// fn tag(env: &Env, sentence: String, hmm: Option<Value>) -> Result<Vector> {
//     let jieba = JIEBA.get_or_init(Jieba::new);
//     let tagged = jieba.tag(sentence.as_str(), hmm.is_some());

//     Ok(tagged
//         .iter()
//         .map(|t| TaggedWord {
//             tag: t.tag.to_owned(),
//             word: t.word.to_owned(),
//         })
//         .collect())
// }

/// Extract the top N keywords from SENTENCE.
/// ALLOWED_POS: a comma-separated list (written as a string) of parts of speech
/// to consider.
///
/// Returns results in the format [(KEYWORD . WEIGHT) ...].
#[defun]
fn extract(env: &Env, sentence: String, n: u32, allowed_pos: Option<String>) -> Result<Vector> {
    let allowed_pos_string = allowed_pos.unwrap_or_else(|| "".to_owned());

    let allowed_pos: Vec<String> = if allowed_pos_string.is_empty() {
        vec![]
    } else {
        allowed_pos_string
            .split(',')
            .map(|s| s.to_owned())
            .collect()
    };

    let keyword_extractor = TFIDF_INSTANCE.get_or_init(|| {
        let jieba = JIEBA.get_or_init(Jieba::new);
        TFIDF::new_with_jieba(jieba)
    });

    let tags = keyword_extractor.extract_tags(sentence.as_str(), n as usize, allowed_pos);

    let vector = env.make_vector(tags.len(), ())?;
    for (i, tag) in tags.into_iter().enumerate() {
        let pair = env.cons(tag.keyword, tag.weight)?;
        vector.set(i, pair)?;
    }
    Ok(vector)
}
