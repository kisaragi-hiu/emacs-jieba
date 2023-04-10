use emacs::{defun, Env, IntoLisp, Result, Value, Vector};
use jieba_rs::{Jieba, Keyword, KeywordExtract, TokenizeMode, TFIDF};
use once_cell::sync::OnceCell;

emacs::plugin_is_GPL_compatible!();

static mut JIEBA: OnceCell<Jieba> = OnceCell::new();
static mut TFIDF_INSTANCE: OnceCell<TFIDF> = OnceCell::new();

// From emacs-tree-sitter
// https://github.com/emacs-tree-sitter/elisp-tree-sitter/blob/master/core/src/query.rs#L13
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

/// Initialize or reset the Jieba instance.
#[defun]
fn load() -> Result<()> {
    unsafe {
        JIEBA = OnceCell::with_value(Jieba::new());
    }
    Ok(())
}

// load_dict: I don't know how to pass a String to a function expecting a BufRead.

/// Add WORD, its FREQUENCY, and its POS to the current instance of Jieba.
#[defun]
fn _add_word(env: &Env, word: String, pos: String, frequency: Option<usize>) -> Result<()> {
    unsafe {
        if let Some(jieba) = JIEBA.get_mut() {
            jieba.add_word(word.as_str(), frequency, Some(pos.as_str()));
        } else {
            env.message("Not yet loaded")?;
        }
    }
    Ok(())
}

/// Cut SENTENCE into tokens.
#[defun]
fn _cut<'e>(env: &'e Env, sentence: String, hmm: Option<Value>) -> Result<Vector<'e>> {
    unsafe {
        let jieba = JIEBA.get_or_init(Jieba::new);
        let cutted = jieba.cut(sentence.as_str(), hmm.is_some());
        vec_to_vector(env, cutted)
    }
}

#[defun]
fn cut_all(env: &Env, sentence: String) -> Result<Vector> {
    unsafe {
        let jieba = JIEBA.get_or_init(Jieba::new);
        let cutted = jieba.cut_all(sentence.as_str());
        vec_to_vector(env, cutted)
    }
}

#[defun]
fn _cut_for_search<'e>(env: &'e Env, sentence: String, hmm: Option<Value>) -> Result<Vector<'e>> {
    unsafe {
        let jieba = JIEBA.get_or_init(Jieba::new);
        let cutted = jieba.cut_for_search(sentence.as_str(), hmm.is_some());
        vec_to_vector(env, cutted)
    }
}

#[defun]
fn _tokenize<'e>(
    env: &'e Env,
    sentence: String,
    mode: String,
    hmm: Option<Value>,
) -> Result<Vector<'e>> {
    unsafe {
        let jieba = JIEBA.get_or_init(Jieba::new);
        let tokens = jieba.tokenize(
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
}

/// Cut SENTENCE into tokens along with parts of speech information.
///
/// Return results in the format [(WORD . TAG) ...].
#[defun]
fn _tag<'e>(env: &'e Env, sentence: String, hmm: Option<Value>) -> Result<Vector<'e>> {
    unsafe {
        let jieba = JIEBA.get_or_init(Jieba::new);
        let tagged = jieba.tag(sentence.as_str(), hmm.is_some());

        let vector = env.make_vector(tagged.len(), ())?;
        for (i, tag) in tagged.iter().enumerate() {
            let pair = env.cons(tag.word.to_owned(), tag.tag.to_owned())?;
            vector.set(i, pair)?;
        }
        Ok(vector)
    }
}

fn internal_extract(sentence: String, n: u32, allowed_pos: Option<String>) -> Vec<Keyword> {
    unsafe {
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

        tags
    }
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
    let vector = env.make_vector(tags.len(), ())?;
    for (i, tag) in tags.into_iter().enumerate() {
        vector.set(i, tag.keyword)?;
    }
    Ok(vector)
}
