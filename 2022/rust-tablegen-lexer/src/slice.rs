// TokenValue holds the actual token; for some types of tokens there's
// associated data, which is a reference to a str with the 'source
// lifetime.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenValue<'source> {
    EOF,
    Error,

    Plus,
    Minus,
    Multiply,
    Divide,
    Period,
    Backslash,
    Colon,
    Percent,
    Pipe,
    Exclamation,
    Question,
    Pound,
    Ampersand,
    Semi,
    Comma,
    LeftParen,
    RightParen,
    LeftAng,
    RightAng,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Equals,
    Comment(&'source str),
    Identifier(&'source str),
    Number(&'source str),
    Quote(&'source str),
}

// Each token is reported with a value and its position as a byte offset in
// the input.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Token<'source> {
    pub value: TokenValue<'source>,
    pub pos: usize,
}

// A Lexer is created with new(), by passing a &str with the input; the lexer's
// lifetime is then tied to the lifetime of this &str. Once you have the lexer,
// next_token() is a low-level API to get new tokens. Lexer also implements
// the Iterator trait for a higher-level iterator API.
pub struct Lexer<'source> {
    input: &'source str,
    iter: Peekable<CharIndices<'source>>,

    // c is the last char taken from iter, and ci is its offset in the input.
    c: char,
    ci: usize,

    // error is true iff the lexer encountered and error.
    error: bool,
}

use core::str::CharIndices;
use std::iter::Peekable;

impl<'source> Lexer<'source> {
    pub fn new(input: &'source str) -> Self {
        let mut lex = Self {
            input,
            iter: input.char_indices().peekable(),
            c: '\x00',
            ci: 0,
            error: false,
        };
        lex.scan_char();
        lex
    }

    // next_token is the "raw" API for Lexers. It yields the next token in the
    // input until it encounters the end, at which point it starts yielding
    // TokenValue::EOF. If it encounters an error, it will return
    // TokenValue::error and will continue returning it for subsequent calls.
    // See also the next() method for an Iterator-like interface.
    pub fn next_token(&mut self) -> Token<'source> {
        self.skip_nontokens();
        if self.is_at_end() {
            return Token {
                value: TokenValue::EOF,
                pos: self.ci,
            };
        }

        // Try to match operators first
        let tokval = match self.c {
            '"' => {
                return self.scan_quote();
            }
            '+' => TokenValue::Plus,
            '-' => TokenValue::Minus,
            '*' => TokenValue::Multiply,
            '/' => {
                if let Some((_, chr)) = self.iter.peek() {
                    if *chr == '/' {
                        return self.scan_comment();
                    }
                }
                TokenValue::Divide
            }
            '.' => TokenValue::Period,
            '\\' => TokenValue::Backslash,
            ':' => TokenValue::Colon,
            '%' => TokenValue::Percent,
            '|' => TokenValue::Pipe,
            '!' => TokenValue::Exclamation,
            '?' => TokenValue::Question,
            '#' => TokenValue::Pound,
            '&' => TokenValue::Ampersand,
            ';' => TokenValue::Semi,
            ',' => TokenValue::Comma,
            '(' => TokenValue::LeftParen,
            ')' => TokenValue::RightParen,
            '<' => TokenValue::LeftAng,
            '>' => TokenValue::RightAng,
            '{' => TokenValue::LeftBrace,
            '}' => TokenValue::RightBrace,
            '[' => TokenValue::LeftBracket,
            ']' => TokenValue::RightBracket,
            '=' => TokenValue::Equals,
            _ => TokenValue::Error,
        };

        if tokval != TokenValue::Error {
            // If an operator matched, return it as a token
            let token = Token {
                value: tokval,
                pos: self.ci,
            };
            self.scan_char();
            token
        } else if self.c.is_alphabetic() || self.c == '_' || self.c == '$' {
            self.scan_identifier()
        } else if self.c.is_digit(10) {
            self.scan_number()
        } else {
            self.error_token()
        }
    }

    // Consumes the next char from source and sets `c` and `ci`. `c` is set
    // to `\x00` as a sentinel value at the end of input.
    fn scan_char(&mut self) {
        if let Some((index, chr)) = self.iter.next() {
            self.ci = index;
            self.c = chr;
        } else {
            self.ci = self.input.len();
            self.c = '\x00';
        }
    }

    fn scan_identifier(&mut self) -> Token<'source> {
        let startpos = self.ci;
        while self.c.is_alphanumeric() || self.c == '_' || self.c == '$' {
            self.scan_char();
        }
        Token {
            value: TokenValue::Identifier(&self.input[startpos..self.ci]),
            pos: startpos,
        }
    }

    fn scan_number(&mut self) -> Token<'source> {
        let startpos = self.ci;
        while self.c.is_digit(10) {
            self.scan_char();
        }
        Token {
            value: TokenValue::Number(&self.input[startpos..self.ci]),
            pos: startpos,
        }
    }

    fn scan_quote(&mut self) -> Token<'source> {
        let startpos = self.ci;
        // Consume leading quote
        self.scan_char();
        while !self.is_at_end() && self.c != '"' {
            self.scan_char();
        }
        if self.c != '"' {
            // Terminating " not found => error
            self.error_token()
        } else {
            // Consume trailing quote
            self.scan_char();
            Token {
                value: TokenValue::Quote(&self.input[startpos..self.ci]),
                pos: startpos,
            }
        }
    }

    fn skip_nontokens(&mut self) {
        while self.c == ' ' || self.c == '\t' || self.c == '\r' || self.c == '\n' {
            self.scan_char()
        }
    }

    fn scan_comment(&mut self) -> Token<'source> {
        let startpos = self.ci;
        while !self.is_at_end() && self.c != '\n' {
            self.scan_char();
        }
        Token {
            value: TokenValue::Comment(&self.input[startpos..self.ci]),
            pos: startpos,
        }
    }

    fn is_at_end(&self) -> bool {
        self.ci >= self.input.len()
    }

    fn error_token(&mut self) -> Token<'source> {
        self.error = true;
        Token {
            value: TokenValue::Error,
            pos: self.ci,
        }
    }
}

// Lexer is an Iterator; it returns tokens until EOF is encountered, when it
// returns None (the EOF token itself is not returned). Note that errors are
// still returned as tokens with TokenValue::Error.
impl<'source> Iterator for Lexer<'source> {
    type Item = Token<'source>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.error {
            // If an error has already been set before we invoke next_token,
            // it means we've already returned TokenValue::Error once and now
            // we should terminate the iteration.
            return None;
        }

        let tok = self.next_token();
        // Fix error tok
        if tok.value == TokenValue::EOF {
            None
        } else {
            Some(tok)
        }
    }
}

// The following functions are used for unit tests and for benchmarking.
use std::{env, fs};

pub fn read_testfile() -> String {
    let filename = env::var("TDINPUT").expect("input env var unspecified");
    let input = fs::read_to_string(filename).expect("cannot read input");
    input
}

pub fn tokenize_all_push<'source>(data: &'source str) -> Vec<Token<'source>> {
    let mut lex = Lexer::new(&data);
    let mut v = vec![];

    loop {
        let t = lex.next_token();
        if t.value == TokenValue::EOF {
            return v;
        }
        v.push(t);
    }
}

pub fn tokenize_all_push_prealloc<'source>(data: &'source str) -> Vec<Token<'source>> {
    let mut lex = Lexer::new(&data);
    let mut v = Vec::with_capacity(200000);

    loop {
        let t = lex.next_token();
        if t.value == TokenValue::EOF {
            return v;
        }
        v.push(t);
    }
}

pub fn tokenize_all_collect<'source>(data: &'source str) -> Vec<Token<'source>> {
    let lex = Lexer::new(&data);
    lex.collect()
}

// Unit tests
#[cfg(test)]
mod tests {
    use super::*;

    // Assert that `tok` has the value `val` and optionally position `pos`.
    macro_rules! assert_tok {
        ($tok:expr, $wantval:expr, $wantpos:expr) => {
            assert_eq!(
                $tok,
                Token {
                    value: $wantval,
                    pos: $wantpos
                }
            );
        };
        ($tok:expr, $wantval:expr) => {
            let tok = $tok;
            assert_eq!(
                tok,
                Token {
                    value: $wantval,
                    pos: tok.pos
                }
            );
        };
    }

    #[test]
    fn simple_iterator() {
        let lex = Lexer::new("hello[]()there");

        let toks: Vec<Token> = lex.collect();
        assert_eq!(toks.len(), 6);
        assert_tok!(toks[0], TokenValue::Identifier("hello"), 0);
        assert_tok!(toks[1], TokenValue::LeftBracket, 5);
        assert_tok!(toks[5], TokenValue::Identifier("there"), 9);
    }

    #[test]
    fn iterator_with_error() {
        // See that the iterator behaves properly with erroneous input.
        // ~ is not a supported token. Iteration will terminate (collect
        // returns successfully) and the last token will be Error.
        let lex = Lexer::new("joe ~ foo");
        let toks: Vec<Token> = lex.collect();
        assert_eq!(toks.len(), 2);
        assert_tok!(toks[0], TokenValue::Identifier("joe"), 0);
        assert_tok!(toks[1], TokenValue::Error, 4);
    }

    #[test]
    fn next_token_with_error() {
        let mut lex = Lexer::new("joe ~ foo");
        assert_tok!(lex.next_token(), TokenValue::Identifier("joe"), 0);
        assert_tok!(lex.next_token(), TokenValue::Error, 4);
        assert_tok!(lex.next_token(), TokenValue::Error, 4);
        assert_tok!(lex.next_token(), TokenValue::Error, 4);
    }

    #[test]
    fn identifiers() {
        let mut lex = Lexer::new("hello_$ j$$");
        assert_tok!(lex.next_token(), TokenValue::Identifier("hello_$"));
        assert_tok!(lex.next_token(), TokenValue::Identifier("j$$"));
    }

    #[test]
    fn operators() {
        let mut lex = Lexer::new("+-&\\;");
        assert_tok!(lex.next_token(), TokenValue::Plus, 0);
        assert_tok!(lex.next_token(), TokenValue::Minus, 1);
        assert_tok!(lex.next_token(), TokenValue::Ampersand, 2);
        assert_tok!(lex.next_token(), TokenValue::Backslash, 3);
        assert_tok!(lex.next_token(), TokenValue::Semi, 4);
        assert_tok!(lex.next_token(), TokenValue::EOF, 5);

        // Test that EOF is sticky
        assert_tok!(lex.next_token(), TokenValue::EOF, 5);
        assert_tok!(lex.next_token(), TokenValue::EOF, 5);
        assert_tok!(lex.next_token(), TokenValue::EOF, 5);
    }

    #[test]
    fn numbers() {
        let mut lex = Lexer::new("10+5090");
        assert_tok!(lex.next_token(), TokenValue::Number("10"), 0);
        assert_tok!(lex.next_token(), TokenValue::Plus, 2);
        assert_tok!(lex.next_token(), TokenValue::Number("5090"), 3);
    }

    #[test]
    fn quote() {
        let mut lex = Lexer::new(r#"joe = "hi";"#);
        assert_tok!(lex.next_token(), TokenValue::Identifier("joe"));
        assert_tok!(lex.next_token(), TokenValue::Equals, 4);
        assert_tok!(lex.next_token(), TokenValue::Quote("\"hi\""), 6);
        assert_tok!(lex.next_token(), TokenValue::Semi, 10);

        let mut lex = Lexer::new(r#"joe = "hi"6"bot""#);
        assert_tok!(lex.next_token(), TokenValue::Identifier("joe"));
        assert_tok!(lex.next_token(), TokenValue::Equals);
        assert_tok!(lex.next_token(), TokenValue::Quote("\"hi\""));
        assert_tok!(lex.next_token(), TokenValue::Number("6"));
        assert_tok!(lex.next_token(), TokenValue::Quote("\"bot\""));

        let mut lex = Lexer::new(r#"let name = "日本語""#);
        assert_tok!(lex.nth(3).unwrap(), TokenValue::Quote("\"日本語\""));
    }

    #[test]
    fn divide_and_comments() {
        // Single '/' is a division
        let mut lex = Lexer::new("foo/bar");
        assert_tok!(lex.next_token(), TokenValue::Identifier("foo"));
        assert_tok!(lex.next_token(), TokenValue::Divide);
        assert_tok!(lex.next_token(), TokenValue::Identifier("bar"));

        // Double '//' is a comment
        let mut lex = Lexer::new("foo//bar");
        assert_tok!(lex.next_token(), TokenValue::Identifier("foo"));
        assert_tok!(lex.next_token(), TokenValue::Comment("//bar"));
        assert_tok!(lex.next_token(), TokenValue::EOF);

        // Comments end at newlines
        let mut lex = Lexer::new("foo//bar\n// comment x\n+");
        assert_tok!(lex.next_token(), TokenValue::Identifier("foo"));
        assert_tok!(lex.next_token(), TokenValue::Comment("//bar"));
        assert_tok!(lex.next_token(), TokenValue::Comment("// comment x"));
        assert_tok!(lex.next_token(), TokenValue::Plus);
        assert_tok!(lex.next_token(), TokenValue::EOF);
    }

    #[test]
    fn large_testfile() {
        // The iterator method doesn't count the last EOF token.
        const WANT_NUM_TOKENS: usize = 168397;

        // Similar testing to the Go version.
        let data = read_testfile();
        let toks = tokenize_all_collect(&data);
        assert_eq!(toks.len(), WANT_NUM_TOKENS);

        let foundpos = toks
            .iter()
            .position(|&t| t.value == TokenValue::Identifier("VCVTf2xsd"))
            .unwrap();

        assert!(matches!(toks[0].value, TokenValue::Comment(_)));
        assert!(matches!(toks[10].value, TokenValue::Comment(_)));
        assert_tok!(toks[11], TokenValue::Identifier("let"));
        assert_tok!(toks[12], TokenValue::Identifier("Component"));
        assert_tok!(toks[13], TokenValue::Equals);
        assert_tok!(toks[14], TokenValue::Quote("\"Sema\""));
        assert_tok!(toks[15], TokenValue::Identifier("in"));
        assert_tok!(toks[16], TokenValue::LeftBrace);

        assert_tok!(toks[foundpos + 1], TokenValue::Colon);
        assert_tok!(toks[foundpos + 2], TokenValue::Identifier("N2VCvtD"));
        assert_tok!(toks[foundpos + 3], TokenValue::LeftAng);
        assert_tok!(toks[foundpos + 4], TokenValue::Number("0"));
        assert_tok!(toks[foundpos + 5], TokenValue::Comma);
        assert_tok!(toks[foundpos + 6], TokenValue::Number("1"));
        assert_tok!(toks[foundpos + 17], TokenValue::Quote("\"s32.f32\""));
    }

    #[test]
    fn compare_tokenize_all() {
        let data = read_testfile();
        let toks1 = tokenize_all_collect(&data);
        let toks2 = tokenize_all_push_prealloc(&data);
        let toks3 = tokenize_all_push(&data);

        assert_eq!(toks1.len(), toks2.len());
        assert_eq!(toks1.len(), toks3.len());

        for i in 0..toks1.len() {
            assert_eq!(toks1[i], toks2[i]);
            assert_eq!(toks1[i], toks3[i]);
        }
    }
}
