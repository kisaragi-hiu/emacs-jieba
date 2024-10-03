#![allow(non_snake_case)]

use std::io::BufReader;

use emacs::{defun, Env, IntoLisp, Result, Value, Vector};
use jieba_rs::{Jieba, Keyword, KeywordExtract, KeywordExtractConfig, TfIdf, TokenizeMode};
use std::sync::{OnceLock, RwLock};

emacs::plugin_is_GPL_compatible!();

static BIG_DICT: &str = include_str!("data/dict.txt.big");

fn jieba() -> &'static RwLock<Jieba> {
    static JIEBA: OnceLock<RwLock<Jieba>> = OnceLock::new();
    JIEBA.get_or_init(|| RwLock::new(Jieba::new()))
}

fn tfidf() -> &'static RwLock<TfIdf> {
    static TFIDF_INSTANCE: OnceLock<RwLock<TfIdf>> = OnceLock::new();
    TFIDF_INSTANCE.get_or_init(|| RwLock::new(TfIdf::default()))
}

// Copied from emacs-tree-sitter
// https://github.com/emacs-tree-sitter/elisp-tree-sitter/blob/master/core/src/query.rs#L13
// Convert a Vec<T> into an Emacs vector. Members of the Vec<T> must be convertable into Lisp types
// (ie. satisfies IntoLisp).
fn vec_to_vector<'e, T: IntoLisp<'e>>(env: &'e Env, vec: Vec<T>) -> Result<Vector<'e>> {
    let vector = env.make_vector(vec.len(), ())?;
    for (i, v) in vec.into_iter().enumerate() {
        vector.set(i, v)?;
    }
    Ok(vector)
}

#[emacs::module(name = "jieba-dyn", defun_prefix = "jieba")]
fn init(_env: &Env) -> Result<()> {
    Ok(())
}

/// Internal reset helper.
/// DICT:
/// - "empty": reset to an empty dict
/// - "big": reset to Jieba's dict.txt.big, which works better with traditional chinese
/// - nil: reset to the default dict
#[defun]
fn _reset(dict: Option<String>) -> Result<()> {
    if let Some(dict) = dict {
        match dict.as_str() {
            "empty" => {
                let mut w = jieba().write().unwrap();
                *w = Jieba::empty();
            }
            "big" => {
                let mut w = jieba().write().unwrap();
                let mut instance = Jieba::empty();
                let mut big_dict = BufReader::new(BIG_DICT.as_bytes());
                instance.load_dict(&mut big_dict).unwrap();
                *w = instance;
            }
            _ => {
                let mut w = jieba().write().unwrap();
                *w = Jieba::new();
            }
        }
    } else {
        let mut w = jieba().write().unwrap();
        *w = Jieba::new();
    };
    Ok(())
}

/// Load DICT, as a string, into the current instance.
#[defun]
fn load_dict(dict: String) -> Result<()> {
    let mut buf = BufReader::new(dict.as_bytes());
    let mut w = jieba().write().unwrap();
    w.load_dict(&mut buf)?;
    Ok(())
}

// load_dict: I don't know how to pass a String to a function expecting a BufRead.

/// Add WORD, its FREQUENCY, and its POS to the current instance of Jieba.
#[defun]
fn _add_word(word: String, pos: String, frequency: Option<usize>) -> Result<()> {
    let mut w = jieba().write().unwrap();
    w.add_word(word.as_str(), frequency, Some(pos.as_str()));
    Ok(())
}

/// Cut SENTENCE into tokens.
#[defun]
fn _cut<'e>(env: &'e Env, sentence: String, hmm: Option<Value>) -> Result<Vector<'e>> {
    let r = jieba().read().unwrap();
    let cutted = r.cut(sentence.as_str(), hmm.is_some());
    vec_to_vector(env, cutted)
}

#[defun]
fn cut_all(env: &Env, sentence: String) -> Result<Vector> {
    let r = jieba().read().unwrap();
    let cutted = r.cut_all(sentence.as_str());
    vec_to_vector(env, cutted)
}

#[defun]
fn _cut_for_search<'e>(env: &'e Env, sentence: String, hmm: Option<Value>) -> Result<Vector<'e>> {
    let r = jieba().read().unwrap();
    let cutted = r.cut_for_search(sentence.as_str(), hmm.is_some());
    vec_to_vector(env, cutted)
}

#[defun]
fn _tokenize<'e>(
    env: &'e Env,
    sentence: String,
    mode: String,
    hmm: Option<Value>,
) -> Result<Vector<'e>> {
    let r = jieba().read().unwrap();
    let tokens = r.tokenize(
        sentence.as_str(),
        match mode.as_str() {
            "search" => TokenizeMode::Search,
            _ => TokenizeMode::Default,
        },
        hmm.is_some(),
    );
    let vector = env.make_vector(tokens.len(), ())?;
    for (i, token) in tokens.into_iter().enumerate() {
        let item = env.list((token.word, token.start, token.end))?;
        vector.set(i, item)?;
    }
    Ok(vector)
}

/// Cut SENTENCE into tokens along with parts of speech information.
///
/// Return results in the format [(WORD . TAG) ...].
#[defun]
fn _tag<'e>(env: &'e Env, sentence: String, hmm: Option<Value>) -> Result<Vector<'e>> {
    let r = jieba().read().unwrap();
    let tagged = r.tag(sentence.as_str(), hmm.is_some());
    let vector = env.make_vector(tagged.len(), ())?;
    for (i, tag) in tagged.iter().enumerate() {
        let pair = env.cons(tag.word.to_owned(), tag.tag.to_owned())?;
        vector.set(i, pair)?;
    }
    Ok(vector)
}

// boilerplate for extract_keywords
fn internal_extract(sentence: String, n: u32, allowed_pos: Option<String>) -> Vec<Keyword> {
    let jieba_instance = jieba().read().unwrap();
    let tfidf_instance = tfidf().read().unwrap();
    let allowed_pos_string = allowed_pos.unwrap_or_else(|| "".to_owned());

    let allowed_pos: Vec<String> = if allowed_pos_string.is_empty() {
        vec![]
    } else {
        allowed_pos_string
            .split(',')
            .map(|s| s.to_owned())
            .collect()
    };

    let tags = tfidf_instance.extract_keywords(
        &jieba_instance,
        sentence.as_str(),
        n as usize,
        allowed_pos,
    );

    tags
}

/// Extract the top N keywords from SENTENCE.
/// ALLOWED_POS: a comma-separated list (written as a string) of parts of speech
/// to consider.
///
/// Return results in the format [(KEYWORD . WEIGHT) ...].
#[defun]
fn _extract(env: &Env, sentence: String, n: u32, allowed_pos: Option<String>) -> Result<Vector> {
    let tags = internal_extract(sentence, n, allowed_pos);
    let vector = env.make_vector(tags.len(), ())?;
    for (i, tag) in tags.into_iter().enumerate() {
        let pair = env.cons(tag.keyword, tag.weight)?;
        vector.set(i, pair)?;
    }
    Ok(vector)
}

/// Extract the top N keywords from SENTENCE.
/// Like `jieba-extract', but weights are discarded.
#[defun]
fn _extract_keywords(
    env: &Env,
    sentence: String,
    n: u32,
    allowed_pos: Option<String>,
) -> Result<Vector> {
    let tags = internal_extract(sentence, n, allowed_pos);
    // We can't use vec_to_vector here because we need `tag.keyword` in the Emacs vector, not just
    // `tag` itself.
    let vector = env.make_vector(tags.len(), ())?;
    for (i, tag) in tags.into_iter().enumerate() {
        vector.set(i, tag.keyword)?;
    }
    Ok(vector)
}

// TODO: stop words? They currently can't really be held (since KeywordExtractConfigBuilder seems to
// not really be public).
