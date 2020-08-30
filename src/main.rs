#[derive(Debug)]
enum Token {
    AddOperator,
    SubOperator,
    MultiplyOperator,
    DivideOperator,
    LeftParenthesis,
    RightParenthesis,
    Value(i64),
}

struct Tokenizer {
    stream: String,
    cursor: usize,
}

impl Tokenizer {
    fn new(s: &str) -> Tokenizer {
        Tokenizer{stream:String::from(s), cursor:0}
    }

    fn c(&mut self) -> Vec<Token> {
        let mut ret = Vec::new();
        while let Some(token) = self.next() {
            ret.push(token);
        }
        return ret;
    }

    fn take_int(&mut self) -> Option<Token> {
        let mut iter = self.stream.chars().skip(self.cursor);

        let mut buffer = String::new();
        fn buffer2int(buffer: String) -> Option<Token> {
            if buffer.is_empty() {
                return None;
            }
            let p = buffer.parse::<i64>();
            let ret = match p {
                Ok(v) => Some(Token::Value(v)),
                _ => None,
            };
            return ret;
        }

        let ret = loop {
            match iter.next() {
                Some(value) => {
                    fn is_number(c: char) -> bool {
                        let candidates = vec!['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
                        candidates.iter().any(|&v| c==v)
                    }            
                    if is_number(value) {
                        self.cursor += 1;
                        buffer.push(value);
                    } else {
                        break buffer2int(buffer);
                    }
                },
                None => {
                    break buffer2int(buffer);
                }
            }
        };
        ret
    }

    fn take_operator(&mut self) -> Option<Token> {
        let mut iter = self.stream.chars().skip(self.cursor);
        match iter.next() {
            Some(value) => {
                self.cursor += 1;
                match value {
                    '+' => Some(Token::AddOperator),
                    '-' => Some(Token::SubOperator),
                    '*' => Some(Token::MultiplyOperator),
                    '/' => Some(Token::DivideOperator),
                    '(' => Some(Token::LeftParenthesis),
                    ')' => Some(Token::RightParenthesis),
                    _ => None,
                }
            },
            None => None,
        }
    }

    fn take_whitespace(&mut self) -> bool {
        let mut iter = self.stream.chars().skip(self.cursor);
        match iter.next() {
            Some(value) => {
                if value.is_whitespace() {
                    self.cursor += 1;
                    true
                } else {
                    false
                }
            },
            None => false
        }
    }
}

impl Iterator for Tokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(token) = self.take_int() {
            Some(token)
        } else if let Some(token) = self.take_operator() {
            Some(token)
        } else {
            if self.take_whitespace() {
                self.next()
            } else {
                None
            }
        }
    }
}


enum Expression {
    AddExpression{
        a: Box<Expression>,
        b: Box<Expression>,
    },
    SubExpression {
        a: Box<Expression>,
        b: Box<Expression>,
    },
    MultiplyExpression {
        a: Box<Expression>,
        b: Box<Expression>
    },
    DivideExpression {
        a: Box<Expression>,
        b: Box<Expression>
    },
    Value(i64),
}

impl Expression {

    fn parse(s: &str) -> Option<Expression> {
        let tokens = Tokenizer::new(s).c();
        Self::parse_expression(&tokens)
    }

    fn parse_expression(tokens: &[Token]) -> Option<Expression> {
        let count = tokens.len();
        if count == 1 {
            return match tokens[0] {
                Token::Value(i) => Some(Expression::Value(i)),
                _ => None,
            }
        }
        for i in 0..count {
            let token = &tokens[i];
            match token {
                Token::AddOperator => {
                    let tokens_head = &tokens[0..i];
                    let tokens_tail = &tokens[i+1..tokens.len()];
                    let head = Self::parse_expression(tokens_head).unwrap();  // TODO better error report
                    let tail = Self::parse_expression(tokens_tail).unwrap();
                    return Some(Expression::AddExpression{a:Box::new(head), b:Box::new(tail)});
                },
                Token::SubOperator => {
                    let tokens_head = &tokens[0..i];
                    let tokens_tail = &tokens[i+1..tokens.len()];
                    let head = Self::parse_expression(tokens_head).unwrap();
                    let tail = Self::parse_expression(tokens_tail).unwrap();
                    return Some(Expression::SubExpression{a:Box::new(head), b:Box::new(tail)});
                },
                Token::MultiplyOperator => {
                    let tokens_head = &tokens[0..i];
                    let tokens_tail = &tokens[i+1..tokens.len()];
                    let head = Self::parse_expression(tokens_head).unwrap();
                    let tail = Self::parse_expression(tokens_tail).unwrap();
                    return Some(Expression::MultiplyExpression{a:Box::new(head), b:Box::new(tail)});
                },
                Token::DivideOperator => {
                    let tokens_head = &tokens[0..i];
                    let tokens_tail = &tokens[i+1..tokens.len()];
                    let head = Self::parse_expression(tokens_head).unwrap();
                    let tail = Self::parse_expression(tokens_tail).unwrap();
                    return Some(Expression::DivideExpression{a:Box::new(head), b:Box::new(tail)});
                },
                _ => {
                    continue;
                }
            }
        }
        panic!("parser error");
    }

    fn interprete(&self) -> i64 {
        match &*self {
            Self::AddExpression{a, b} => {
                a.interprete() + b.interprete()
            },
            Self::SubExpression{a, b} => {
                a.interprete() - b.interprete()
            },
            Self::MultiplyExpression{a, b} => {
                a.interprete() * b.interprete()
            },
            Self::DivideExpression{a, b} => {
                a.interprete() / b.interprete()
            },
            Self::Value(a) => {
                *a
            }
        }
    }
}

pub fn calculate(s: &str) -> Result<i64, &str> {
    let parse_result = Expression::parse(s);
    match parse_result {
        Some(expression) => Ok(expression.interprete()),
        None => Err("parse failed")
    }
}

#[test]
fn calculate_test() {
    assert_eq!(calculate("1+1").unwrap(), 2);
    assert_eq!(calculate("5*8").unwrap(), 40);
    assert_eq!(calculate("6/3").unwrap(), 2);
    assert_eq!(calculate("1+2*3").unwrap(), 7);
}

#[test]
fn tokenize_test() {
    let tokenizer = Tokenizer::new("1+2*3");
    for token in tokenizer {
        println!("{:?}", token);
    }
}

fn main() {
    let r = calculate("1+1").unwrap();
    println!("Hello, world! {}", r);
}
