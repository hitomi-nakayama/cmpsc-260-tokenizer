use std::collections::VecDeque;
use std::io::BufRead;

pub struct SourceReader<'a> {
    line_number: usize,
    line_reader: Box<dyn BufRead + 'a>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct LineResult {
    line_number: usize,
    text: String,
}

impl<'a> SourceReader<'a> {
    fn new<T: BufRead + 'a>(line_reader: T) -> Self {
        let reader = SourceReader {
            line_reader: Box::new(line_reader),
            line_number: 0
        };
        reader
    }

    fn read_line(&mut self) -> Option<LineResult> {
        self.line_number += 1;

        let mut line = String::new();

        let result = self.line_reader.read_line(&mut line).unwrap();
        if result == 0 {
            return None;
        }

        line = line.trim_end().to_owned();

        Some(LineResult {
            line_number: self.line_number,
            text: line
        })
    }
}

pub struct TokenReader<'a> {
    out_of_lines: bool,
    last_read_line_number: usize,
    tokens: VecDeque<(String, usize)>,
    source_reader: Option<SourceReader<'a>>,
    special_tokens: Vec<String>
}

impl<'a> TokenReader<'a> {
    pub fn new(source_reader: SourceReader<'a>, special_tokens: &[&str]) -> Self {
        let special_tokens: Vec<_> = special_tokens.iter().map(|x| (*x).to_owned()).collect();
        let mut tokens = TokenReader {
            out_of_lines: false,
            last_read_line_number: 0,
            tokens: VecDeque::new(),
            source_reader: Some(source_reader),
            special_tokens
        };
        tokens.append_buffer(1);
        tokens
    }

    pub fn from_buf_read<T: BufRead + 'a>(buf_read: T, special_tokens: &[&str]) -> Self {
        let source_reader = SourceReader::new(buf_read);
        Self::new(source_reader, special_tokens)
    }

    pub fn from_tokens(tokens: Vec<String>) -> Self {
        let tokens: VecDeque<_> = tokens.into_iter().map(|x| (x, 1 as usize)).collect();
        let tokens = TokenReader {
            out_of_lines: true,
            last_read_line_number: 0,
            source_reader: None,
            tokens,
            special_tokens: Vec::new(),
        };
        tokens
    }

    pub fn is_empty(&self) -> bool {
        self.out_of_lines && self.tokens.is_empty()
    }

    pub fn peek(&self) -> Option<&str> {
        self.tokens.front().map(|x| x.0.as_str())
    }

    pub fn peek_n(&mut self, n: usize) -> Option<&str> {
        self.append_buffer(n);
        self.tokens.get(n).map(|x| x.0.as_str())
    }

    pub fn line_number(&self) -> usize {
        self.tokens.front().map(|x| x.1).unwrap_or(self.last_read_line_number)
    }

    pub fn take(&mut self) -> Option<String> {
        self.append_buffer(1);
        let result = self.tokens.pop_front().map(|x| x.0.to_owned());
        self.append_buffer(1);
        result
    }

    fn append_buffer(&mut self, count: usize) {
        let special_tokens: Vec<_> = self.special_tokens.iter().map(|x| x.as_str()).collect();
        while !self.out_of_lines && self.tokens.len() < count {
            if let Some(source_reader) = &mut self.source_reader {
                if let Some(line) = source_reader.read_line() {
                    self.last_read_line_number = line.line_number;
                    let tokens = tokenize_line(&line.text, &special_tokens);
                    let tokens = tokens.into_iter().map(|x| (x, line.line_number));
                    self.tokens.extend(tokens);
                } else {
                    self.out_of_lines = true;
                }
            } else {
                self.out_of_lines = true;
            }
        }
    }
}

enum TokenizerState {
    Start,
    ReParse,
    InWord(usize),
    InSpecial(usize)
}

/** Read the next non-empty line from the reader and tokenize it. */
fn tokenize_line(input: &str, special: &[&str]) -> Vec<String> {
    use TokenizerState::*;

    let mut tokens: Vec<String> = Vec::new();
    let mut state = Start;

    let mut emit = |begin, end| tokens.push(input[begin..end].to_string());

    // add space so we can handle the last word
    let text = input.chars().chain(" ".chars());
    for (i, c) in text.enumerate() {
        loop {
            state = if c.is_whitespace() {
                match state {
                    InWord(begin)
                    | InSpecial(begin) => {
                        emit(begin, i);
                    },
                    _ => {}
                }
                Start
            } else {
                match state {
                    Start => {
                        if is_start_of_special(c, special) {
                            InSpecial(i)
                        } else {
                            InWord(i)
                        }
                    },
                    InWord(begin) => {
                        if is_start_of_special(c, special) {
                            emit(begin, i);
                            ReParse
                        } else {
                            state
                        }
                    },
                    InSpecial(begin) => {
                        let substr = &input[begin..i+1];
                        let is_special = special.iter()
                            .any(|x| x.starts_with(substr));
                        if !(is_special) {
                            emit(begin, i);
                            ReParse
                        } else {
                            state
                        }
                    },
                    ReParse => panic!("Unexpected ReParse state!")
                }
            };
            state = match state {
                ReParse => Start,
                _ => break
            }
        }
    }
    tokens
}

fn is_start_of_special(c: char, special: &[&str]) -> bool {
    return special
        .iter()
        .any(|x| x.starts_with(c))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_line_statement() {
        let special = vec![",", "(", ")", "{", "}", "=", ":", "->"];
        let tokens = tokenize_line("div:int = $arith div i2:int i3:int", &special);
        assert_eq!(tokens, vec!["div", ":", "int", "=", "$arith", "div", "i2", ":", "int", "i3", ":", "int"]);
    }

    #[test]
    fn tokenize_line_function_call() {
        let special = vec![",", "(", ")", "{", "}", "=", ":", "->"];
        let tokens = tokenize_line("call3:int = $call input()", &special);
        assert_eq!(tokens, vec!["call3", ":", "int", "=", "$call", "input", "(", ")"]);
    }

    #[test]
    fn tokenize_line_function_declaration() {
        let special = vec![",", "(", ")", "{", "}", "=", ":", "->"];
        let expected = vec!["function", "construct", "(", "value", ":", "int", ",", "left", ":", "TreeNode*", ")", "->", "TreeNode*", "{"];
        let actual = tokenize_line("function construct(value:int, left:TreeNode*) -> TreeNode* {", &special);
        assert_eq!(expected, actual);
    }

    #[test]
    fn tokenize_line_indented() {
        let special = Vec::new();
        let tokens = tokenize_line("  a b c", &special);
        assert_eq!(tokens, vec!["a", "b", "c"]);
    }

    #[test]
    fn source_reader() {
        let source = "a b c
d e f
";

        let line_0_expected = "a b c";
        let line_1_expected = "d e f";

        let mut reader = SourceReader::new(source.as_bytes());

        let line_0_actual = reader.read_line().unwrap().text;
        assert_eq!(line_0_expected, line_0_actual);

        let line_1_actual = reader.read_line().unwrap().text;
        assert_eq!(line_1_expected, line_1_actual);

        assert!(reader.read_line().is_none());
    }

}
