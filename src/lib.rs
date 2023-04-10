use emacs::{defun, Env, Result, Value, Vector};
use jieba_rs::{Jieba, KeywordExtract, TFIDF};
use once_cell::sync::OnceCell;
use std::str;

emacs::plugin_is_GPL_compatible!();

static JIEBA: OnceCell<Jieba> = OnceCell::new();
static TFIDF_INSTANCE: OnceCell<Jieba> = OnceCell::new();

#[emacs::module(name = "jieba")]
fn init(env: &Env) -> Result<()> {
    Ok(())
}

#[inline]
fn assert_not_init() -> Result<()> {
    if JIEBA.get().is_some() {
        Err(Error::from_reason(
            "Jieba has already been loaded, cannot load again".to_owned(),
        ))
    } else {
        Ok(())
    }
}

/// Initialize Jieba, or do nothing if it is already initialized.
#[defun]
fn load() -> Result<()> {
    assert_not_init()?;
    let _ = JIEBA.get_or_init(Jieba::new);
    Ok(())
}

/// Cut SENTENCE into an array of tokens.
#[defun]
fn cut(env: &Env, sentence: String, hmm: Option<Value>) -> Result<Vector<String>> {
    let jieba = JIEBA.get_or_init(Jieba::new);
    let cutted = jieba.cut(sentence.as_str(), hmm.is_some());
    Ok(env.vector(cutted))
}

#[defun]
fn cut_all(env: &Env, sentence: String) -> Result<Vector<String>> {
    let jieba = JIEBA.get_or_init(Jieba::new);
    let cutted = jieba.cut_all(sentence.as_str());
    Ok(env.vector(cutted))
}

#[defun]
fn cut_for_search(env: &Env, sentence: String, hmm: Option<Value>) -> Result<Vector<String>> {
    let jieba = JIEBA.get_or_init(Jieba::new);
    let cutted = jieba.cut_for_search(sentence.as_str(), hmm.is_some());
    Ok(env.vector(cutted))
}

struct TaggedWord {
    pub tag: String,
    pub word: String,
}

#[defun]
fn tag(env: &Env, sentence: String, hmm: Option<Value>) -> Result<Vector<TaggedWord>> {
    let jieba = JIEBA.get_or_init(Jieba::new);
    let tagged = jieba.tag(sentence.as_str(), hmm.is_some());

    Ok(tagged
        .iter()
        .map(|t| TaggedWord {
            tag: t.tag.to_owned(),
            word: t.word.to_owned(),
        })
        .collect())
}

struct Keyword {
    pub keyword: String,
    pub weight: f64,
}

#[defun]
fn extract(sentence: String, topn: u32, allowed_pos: Option<String>) -> Result<Vector<Keyword>> {
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

    let tags = keyword_extractor.extract_tags(sentence.as_str(), topn as usize, allowed_pos);

    Ok(tags
        .into_iter()
        .map(|tag| Keyword {
            keyword: tag.keyword,
            weight: tag.weight,
        })
        .collect::<Vector<Keyword>>())
}
