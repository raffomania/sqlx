//! Parsing support for Generalized Query Placeholders, similar to `println!()` or `format_args!()` syntax.
//!
//! Examples:
//!
//! Implicit indexing: `SELECT * FROM foo WHERE id = {} AND bar = {}`
//! where each placeholder implicitly refers to an expression at the equivalent position
//! in the bind arguments list
//!
//! Explicit zero-based indexing: `SELECT * FROM foo WHERE id = {N}` where `N` is an unsigned integer
//! which refers to the Nth expression in the bind arguments list (starting from zero)
//!
//! Arguments capturing:
//!
//! `SELECT * FROM foo WHERE id = {<ident>}` where `<ident>` is a Rust identifier
//! defined in the same scope as the query string (for the macros) or an explicitly named bind argument
//! (for the dynamic interface)
//!
//! `SELECT * FROM foo WHERE id = {<field-expr>}` where `<field-expr>` is a Rust field expression
//! (e.g. `foo.bar.baz`) which resolves in the current scope (for the macros)
//!
//!
//! Repetition interpolated into the query string
//! * `SELECT * FROM foo WHERE id IN ({,*})`
//! * `SELECT * FROM foo WHERE id IN ({N,*})`
//! * `SELECT * FROM foo WHERE id IN ({<ident>,*})`
//! * `SELECT * FROM foo WHERE id IN ({(<field-expr>),*})`
//!
//! Similar to the above, but where the bind argument corresponding to the placeholder is expected
//! to be an iterable, and the repetition is expanded into the query string at runtime
//! (for databases which don't support binding arrays).
//!
//! For example:
//!
//! ```rust,ignore
//! let foo = [1, 2, 3, 4, 5];
//!
//! sqlx::query!("SELECT * FROM foo WHERE id IN ({foo,*}")
//!
//! // would be equivalent to:
//!
//! sqlx::query!("SELECT * FROM foo WHERE id IN ($1, $2, $3, $4, $5)", foo[0], foo[1], foo[2], foo[3], foo[4])
//! ```
//!
//! (Note: for Postgres, binding the array directly instead of using expansion should be preferred
//! as it will not generate a different query string for every arity of iterable passed.)
//!
//! ### Potential Pitfalls to Avoid
//! We want to make sure to avoid trying to parse paired braces inside strings as it could
//! be, e.g. a JSON object literal. We also need to support escaping braces (and erasing the escapes)
//!
use std::borrow::Cow;
use std::convert::TryInto;
use std::ops::Range;

type Result<T, E = ParseError> = std::result::Result<T, E>;

#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct ParseError {
    pub pos: usize,
    pub reason: String,
}

pub struct ParsedQuery<'a> {
    pub(crate) query: &'a str,
    pub(crate) placeholders: Vec<Placeholder<'a>>,
}

#[derive(Clone, Debug)]
pub struct Placeholder<'a> {
    pub token: Range<usize>,
    pub ident: Ident<'a>,
    pub kleene: Option<Kleene>,
}

#[derive(Clone, Debug)]
pub enum Ident<'a> {
    Positional(u16),
    Named(Cow<'a, str>),
}

impl Ident<'_> {
    fn into_static(self) -> Ident<'static> {
        match self {
            Self::Positional(pos) => Self::Positional(pos),
            Self::Named(named) => Self::Named(named.into_owned().into()),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Kleene {
    Question,
    Star,
    Plus,
}

struct ParseCtxt<'a> {
    query: &'a str,
    pos: usize,
    placeholders: Vec<Placeholder<'a>>,
}

impl<'a> ParseCtxt<'a> {
    fn new(query: &'a str) -> Self {
        ParseCtxt { query, pos: 0, placeholders: vec![] }
    }

    fn run(mut self) -> Result<Vec<Placeholder<'a>>> {
        while self.pos < self.query.len() {
            if let Some(i) = self.query[self.pos..].find(&['{', '\'', '"', '`'][..]) {
                self.pos += i;

                if self.query[self.pos] == '{' {
                    self.placeholders.push(self.parse_placeholder()?);
                } else {
                    self.consume_string()?;
                }
            } else {
                break;
            }
        }

        Ok(self.placeholders)
    }

    /// Find the paired quote for the character at `self.query[self.pos]` and skip past it
    fn consume_string(&mut self) -> Result<()> {
        let start = self.pos;
        let delim = self.query[self.pos];

        while let Some(i) = self.query[self.pos + 1..].find(delim) {
            // the current position in `self.query`
            self.pos = self.pos + i + 1;

            if self.query[self.pos - 1] == '\\' {
                self.pos += 1;
                continue;
            }
        }

        Err(ParseError {
            pos: start,
            reason: format!("unpaired delimiter: '{}'", self.query[start]),
        })
    }

    fn parse_placeholder(&mut self) -> Result<Placeholder<'a>> {
        let start = self.pos;
        self.pos += 1;

        let (next_idx, next) =
            self.query[self.pos..].trim_start().char_indices().next().ok_or_else(|| {
                ParseError { pos: start, reason: "unpaired delimiter: '{'".into() }
            })?;

        self.pos += next_idx;

        let ident = match next {
            '}' => {
                let idx: u16 = (self.placeholders.len() + 1).try_into().map_err(|_| {
                    ParseError { pos: start, reason: "placeholder limit exceeded".into() }
                })?;

                // advance beyond the placeholder
                self.pos += 1;

                return Ok(Placeholder {
                    token: start..self.pos,
                    ident: Ident::Positional(idx),
                    kleene: None,
                });
            }
            _ if next.is_digit(10) => Ident::Positional(self.parse_positional_index()?),
            _ if next == '_' || next.is_alphabetic() => {
                Ident::Named(self.parse_named_ident()?.into())
            }
            _ => {
                return Err(ParseError {
                    pos: self.pos,
                    reason: format!("unexpected character: '{}'", next),
                })
            }
        };
    }

    fn parse_positional_index(&mut self) -> Result<u16> {
        let start = self.pos;
        if let Some(end) = self.query[self.pos].find(|c: char| !c.is_digit(10)) {
            self.pos += end;
            self.query[start..self.pos]
                .parse::<u16>()
                .map_err(|e| ParseError { pos: start, reason: e.to_string() })
        } else {
            Err(ParseError { pos: start, reason: "unexpected end of query string".into() })
        }
    }

    fn parse_named_ident(&mut self) -> Result<&'a str> {}
}

pub fn parse_query(query: &str) -> Result<ParsedQuery> {
    let placeholders = ParseCtxt::new(query).run()?;

    Ok(ParsedQuery { query, placeholders })
}
