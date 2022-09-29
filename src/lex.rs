use crate::error::Error;

use lazy_static::lazy_static;
use regex::Regex;
use std::{fmt, fs};

// TODO: add byte literal (eg b'a')

#[derive(Clone, Copy)]
pub struct Span {
    pub text: &'static str,
    pub line: usize,
    pub column: usize,
    pub code: &'static str,
    pub file: &'static str,
}

impl Span {
    pub fn line(&self) -> &'static str {
        self.code
            .lines()
            .nth(self.line)
            .unwrap_or(&self.code[self.code.len()..])
    }

    pub fn line_prefix(&self) -> &'static str {
        let line = self.line();
        let line_start = line.as_ptr();
        let text_start = self.text.as_ptr();
        let len = text_start as usize - line_start as usize;
        unsafe {
            let slice = std::slice::from_raw_parts(line_start, len);
            std::str::from_utf8_unchecked(slice)
        }
    }

    pub fn empty() -> Span {
        Span {
            text: "",
            line: 0,
            column: 0,
            code: "",
            file: "",
        }
    }
}

#[derive(Clone, Debug)]
pub enum Token {
    Int(i64, Span),
    Float(f64, Span),
    Bool(bool, Span),
    Char(char, Span),
    String(String, Span),

    Name(Span),

    Arrow(Span),
    LParen(Span),
    RParen(Span),
    LBrace(Span),
    RBrace(Span),
    LBrack(Span),
    RBrack(Span),

    Fn(Span),
    If(Span),
    Else(Span),
    While(Span),
    For(Span),
    To(Span),
    Let(Span),

    // import
    // struct
    // const
    // global
    Eof(Span),
}

impl Token {
    pub fn span(&self) -> Span {
        match self {
            Token::Int(_, span) => *span,
            Token::Float(_, span) => *span,
            Token::Bool(_, span) => *span,
            Token::Char(_, span) => *span,
            Token::String(_, span) => *span,
            Token::Name(span) => *span,
            Token::Arrow(span) => *span,
            Token::LParen(span) => *span,
            Token::RParen(span) => *span,
            Token::LBrace(span) => *span,
            Token::RBrace(span) => *span,
            Token::LBrack(span) => *span,
            Token::RBrack(span) => *span,
            Token::Fn(span) => *span,
            Token::If(span) => *span,
            Token::Else(span) => *span,
            Token::While(span) => *span,
            Token::For(span) => *span,
            Token::To(span) => *span,
            Token::Let(span) => *span,
            Token::Eof(span) => *span,
        }
    }

    pub fn is_name(&self) -> bool {
        matches!(self, Token::Name(_))
    }

    pub fn is_lbrace(&self) -> bool {
        matches!(self, Token::LBrace(_))
    }

    pub fn is_rbrace(&self) -> bool {
        matches!(self, Token::RBrace(_))
    }

    pub fn is_lparen(&self) -> bool {
        matches!(self, Token::LParen(_))
    }

    pub fn is_lbrack(&self) -> bool {
        matches!(self, Token::LBrack(_))
    }

    pub fn is_rbrack(&self) -> bool {
        matches!(self, Token::RBrack(_))
    }

    pub fn is_arrow(&self) -> bool {
        matches!(self, Token::Arrow(_))
    }

    pub fn is_if(&self) -> bool {
        matches!(self, Token::If(_))
    }

    pub fn is_else(&self) -> bool {
        matches!(self, Token::Else(_))
    }

    pub fn is_for(&self) -> bool {
        matches!(self, Token::For(_))
    }

    pub fn is_to(&self) -> bool {
        matches!(self, Token::To(_))
    }

    pub fn is_while(&self) -> bool {
        matches!(self, Token::While(_))
    }

    pub fn is_let(&self) -> bool {
        matches!(self, Token::Let(_))
    }

    pub fn is_standalone(&self) -> bool {
        matches!(
            self,
            Token::Name(_)
                | Token::Int(_, _)
                | Token::Float(_, _)
                | Token::Bool(_, _)
                | Token::Char(_, _)
                | Token::String(_, _)
        )
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Span({:?}, {}, {})",
            self.text,
            self.line + 1,
            self.column + 1
        )
    }
}

pub struct Tokens {
    iter: TokenIter,
    peek: Token,
}

impl Tokens {
    pub fn from_file(path: &str) -> Result<Tokens, Error> {
        let mut iter = TokenIter::from_file(path)?;
        let peek = iter.next()?;
        Ok(Tokens { iter, peek })
    }

    pub fn next(&mut self) -> Result<Token, Error> {
        Ok(std::mem::replace(&mut self.peek, self.iter.next()?))
    }

    pub fn peek(&self) -> &Token {
        &self.peek
    }
}

struct TokenIter {
    suffix: &'static str,
    line: usize,
    column: usize,
    code: &'static str,
    file: &'static str,
}

impl TokenIter {
    fn from_file(path: &str) -> Result<TokenIter, Error> {
        if let Ok(code) = fs::read_to_string(path) {
            let code = Box::leak(Box::new(code));
            let file = Box::leak(Box::new(path.to_string()));
            Ok(TokenIter {
                suffix: code,
                line: 0,
                column: 0,
                code,
                file,
            })
        } else {
            Err(Error::Io(format!("cannot open {}", path)))
        }
    }

    fn next(&mut self) -> Result<Token, Error> {
        if self.suffix.is_empty() {
            Ok(Token::Eof(Span {
                text: self.suffix,
                line: self.line,
                column: self.column,
                code: self.code,
                file: self.file,
            }))
        } else if self.suffix.starts_with(char::is_whitespace) {
            self.skip_whitespace();
            self.next()
        } else {
            self.next_token()
        }
    }

    fn make_span(&mut self, len: usize) -> Span {
        let span = Span {
            text: &self.suffix[..len],
            line: self.line,
            column: self.column,
            code: self.code,
            file: self.file,
        };

        self.suffix = &self.suffix[len..];

        for c in span.text.chars() {
            if c == '\n' {
                self.line += 1;
                self.column = 0;
            } else {
                self.column += 1;
            }
        }

        span
    }

    fn skip_whitespace(&mut self) {
        let mut len = 1;
        while self.suffix[len..].starts_with(char::is_whitespace) {
            len += 1;
        }
        self.make_span(len);
    }

    fn next_token(&mut self) -> Result<Token, Error> {
        if let Some(token) = self.try_to_lex_number() {
            return token;
        }

        if self.suffix.starts_with("->") {
            return Ok(Token::Arrow(self.make_span(2)));
        } else if self.suffix.starts_with('(') {
            return Ok(Token::LParen(self.make_span(1)));
        } else if self.suffix.starts_with(')') {
            return Ok(Token::RParen(self.make_span(1)));
        } else if self.suffix.starts_with('{') {
            return Ok(Token::LBrace(self.make_span(1)));
        } else if self.suffix.starts_with('}') {
            return Ok(Token::RBrace(self.make_span(1)));
        } else if self.suffix.starts_with('[') {
            return Ok(Token::LBrack(self.make_span(1)));
        } else if self.suffix.starts_with(']') {
            return Ok(Token::RBrack(self.make_span(1)));
        } else if self.suffix.starts_with('"') {
            return self.lex_string();
        } else if self.suffix.starts_with('\'') {
            return self.lex_char();
        } else if self.suffix.starts_with("//") {
            let mut len = 0;
            for c in self.suffix.chars() {
                if c == '\n' {
                    len += 1;
                    break;
                } else {
                    len += c.len_utf8();
                }
            }
            self.make_span(len);
            return self.next();
        }

        let mut len = 0;

        // TODO: lex .field and ..field
        if self.suffix.starts_with(is_normal_start) {
            while self.suffix[len..].starts_with(is_normal_continue) {
                len += self.suffix.chars().next().unwrap().len_utf8();
            }
        } else {
            while is_symbol_continue(&self.suffix[len..]) {
                len += self.suffix.chars().next().unwrap().len_utf8();
            }
        }

        let span = self.make_span(len);
        Ok(match span.text {
            "fn" => Token::Fn(span),
            "if" => Token::If(span),
            "else" => Token::Else(span),
            "while" => Token::While(span),
            "for" => Token::For(span),
            "to" => Token::To(span),
            "let" => Token::Let(span),
            "true" => Token::Bool(true, span),
            "false" => Token::Bool(false, span),
            _ => Token::Name(span),
        })
    }

    fn try_to_lex_number(&mut self) -> Option<Result<Token, Error>> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"\A-?((\d*\.\d+)|(\d+\.?))([Ee][+-]?\d+)?").unwrap();
        }

        let len = RE.find(self.suffix)?.end();
        let span = self.make_span(len);

        Some(
            if span.text.chars().any(|c| c == '.' || c == 'e' || c == 'E') {
                span.text
                    .parse::<f64>()
                    .map(|f| Token::Float(f, span))
                    .map_err(|_| Error::Lex(span, "float literal is too big".to_string()))
            } else if let Ok(i) = span.text.parse::<i64>() {
                Ok(Token::Int(i, span))
            } else {
                Err(Error::Lex(span, "int literal is too big".to_string()))
            },
        )
    }

    fn lex_string(&mut self) -> Result<Token, Error> {
        let mut string = String::new();
        let mut len = 1;
        while let Some((c, l)) = self.lex_char_or_escape(&self.suffix[len..], len, '"')? {
            string.push(c);
            len += l;
        }
        Ok(Token::String(string, self.make_span(len + 1)))
    }

    fn lex_char(&mut self) -> Result<Token, Error> {
        if let Some((c, len)) = self.lex_char_or_escape(&self.suffix[1..], 1, '\'')? {
            if self.suffix[1 + len..].starts_with('\'') {
                Ok(Token::Char(c, self.make_span(len + 2)))
            } else {
                Err(Error::Lex(
                    self.make_span(1),
                    "unterminated char literal".to_string(),
                ))
            }
        } else {
            Err(Error::Lex(
                self.make_span(2),
                "empty char literal".to_string(),
            ))
        }
    }

    fn lex_char_or_escape(
        &mut self,
        suffix: &str,
        len: usize,
        end: char,
    ) -> Result<Option<(char, usize)>, Error> {
        let unterminated_msg = if end == '"' {
            "unterminated string literal".to_string()
        } else {
            "unterminated char literal".to_string()
        };
        let mut chars = suffix.chars();
        if let Some(c) = chars.next() {
            if c == end {
                Ok(None)
            } else if c == '\\' {
                match chars.next() {
                    Some('\\') => Ok(Some(('\\', 2))),
                    Some('n') => Ok(Some(('\n', 2))),
                    Some('r') => Ok(Some(('\r', 2))),
                    Some('t') => Ok(Some(('\t', 2))),
                    Some('u') => self.lex_hex_digits::<4, 'u'>(suffix, len),
                    Some('U') => self.lex_hex_digits::<8, 'U'>(suffix, len),
                    Some('x') => self.lex_hex_digits::<2, 'x'>(suffix, len),
                    Some('0') => Ok(Some((0 as char, 2))),
                    Some(c) if c == end => Ok(Some((end, 2))),
                    Some(unknown) => {
                        self.make_span(len);
                        Err(Error::Lex(
                            self.make_span(1 + unknown.len_utf8()),
                            "unknown escape character".to_string(),
                        ))
                    }
                    None => Err(Error::Lex(self.make_span(1), unterminated_msg)),
                }
            } else {
                Ok(Some((c, c.len_utf8())))
            }
        } else {
            Err(Error::Lex(self.make_span(1), unterminated_msg))
        }
    }

    fn lex_hex_digits<const N: usize, const C: char>(
        &mut self,
        suffix: &str,
        len: usize,
    ) -> Result<Option<(char, usize)>, Error> {
        let mut chars = suffix[2..].chars();
        let mut bytes = [0; N];

        #[allow(clippy::needless_range_loop)]
        for i in 0..N {
            if let Some(c) = chars.next() {
                if c.is_ascii_hexdigit() {
                    bytes[i] = c as u8;
                } else {
                    self.make_span(len);
                    return Err(Error::Lex(
                        self.make_span(3 + i),
                        format!("'\\{C}' must be followed by {N} hex digits"),
                    ));
                }
            } else {
                self.make_span(len);
                return Err(Error::Lex(
                    self.make_span(2 + i),
                    format!("'\\{C}' must be followed by {N} hex digits"),
                ));
            }
        }

        let s = std::str::from_utf8(&bytes).unwrap();
        let i = u32::from_str_radix(s, 16).unwrap();

        if let Some(c) = char::from_u32(i) {
            Ok(Some((c, 2 + N)))
        } else {
            self.make_span(len);
            Err(Error::Lex(
                self.make_span(2 + N),
                "invalid unicode code point".to_string(),
            ))
        }
    }
}

pub fn is_normal_start(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn is_normal_continue(c: char) -> bool {
    c.is_ascii_digit() || c.is_alphabetic() || c == '_'
}

fn is_symbol_continue(suffix: &str) -> bool {
    !suffix.starts_with(char::is_whitespace)
        && !suffix.starts_with(char::is_control)
        && !suffix.starts_with("->")
        && !suffix.starts_with('(')
        && !suffix.starts_with(')')
        && !suffix.starts_with('{')
        && !suffix.starts_with('}')
        && !suffix.starts_with('[')
        && !suffix.starts_with(']')
        && !suffix.starts_with('"')
        && !suffix.starts_with('\'')
        && !suffix.starts_with("//")
        && !suffix.starts_with(char::is_alphabetic)
        && !suffix.starts_with(|c: char| c.is_ascii_digit())
        && !suffix.starts_with('_')
}
