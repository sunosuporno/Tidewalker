#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Expr {
    Path(String),
    Number(String),
    Bool(bool),
    Address(String),
    Ref {
        mutable: bool,
        expr: Box<Expr>,
    },
    Field {
        base: Box<Expr>,
        field: String,
    },
    Call {
        func: String,
        args: Vec<Expr>,
    },
    Cast {
        expr: Box<Expr>,
        ty: String,
    },
    Binary {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum BinOp {
    Or,
    And,
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
}

impl BinOp {
    pub(super) fn as_str(self) -> &'static str {
        match self {
            BinOp::Or => "||",
            BinOp::And => "&&",
            BinOp::Eq => "==",
            BinOp::Ne => "!=",
            BinOp::Gt => ">",
            BinOp::Lt => "<",
            BinOp::Ge => ">=",
            BinOp::Le => "<=",
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Shl => "<<",
            BinOp::Shr => ">>",
        }
    }

    fn binding_power(self) -> (u8, u8) {
        match self {
            BinOp::Or => (1, 2),
            BinOp::And => (3, 4),
            BinOp::Eq | BinOp::Ne | BinOp::Gt | BinOp::Lt | BinOp::Ge | BinOp::Le => (5, 6),
            BinOp::Add | BinOp::Sub | BinOp::Shl | BinOp::Shr => (7, 8),
            BinOp::Mul | BinOp::Div | BinOp::Mod => (9, 10),
        }
    }

    pub(super) fn is_comparison(self) -> bool {
        matches!(
            self,
            BinOp::Eq | BinOp::Ne | BinOp::Gt | BinOp::Lt | BinOp::Ge | BinOp::Le
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Tok {
    Ident(String),
    Number(String),
    Address(String),
    True,
    False,
    LParen,
    RParen,
    Comma,
    Dot,
    ColonColon,
    Amp,
    KeywordMut,
    KeywordAs,
    Op(BinOp),
}

struct Lexer<'a> {
    input: &'a str,
    chars: Vec<char>,
    idx: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.chars().collect(),
            idx: 0,
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.idx).copied()
    }

    fn peek2(&self) -> Option<(char, char)> {
        let a = self.chars.get(self.idx).copied()?;
        let b = self.chars.get(self.idx + 1).copied()?;
        Some((a, b))
    }

    fn bump(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.idx += 1;
        Some(ch)
    }

    fn take_while<F: Fn(char) -> bool>(&mut self, pred: F) -> String {
        let mut out = String::new();
        while let Some(ch) = self.peek() {
            if !pred(ch) {
                break;
            }
            out.push(ch);
            self.idx += 1;
        }
        out
    }

    fn lex(mut self) -> Option<Vec<Tok>> {
        let _ = self.input;
        let mut out = Vec::new();
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.bump();
                continue;
            }

            if let Some((a, b)) = self.peek2() {
                let two = match (a, b) {
                    (':', ':') => Some(Tok::ColonColon),
                    ('&', '&') => Some(Tok::Op(BinOp::And)),
                    ('|', '|') => Some(Tok::Op(BinOp::Or)),
                    ('=', '=') => Some(Tok::Op(BinOp::Eq)),
                    ('!', '=') => Some(Tok::Op(BinOp::Ne)),
                    ('>', '=') => Some(Tok::Op(BinOp::Ge)),
                    ('<', '=') => Some(Tok::Op(BinOp::Le)),
                    ('<', '<') => Some(Tok::Op(BinOp::Shl)),
                    ('>', '>') => Some(Tok::Op(BinOp::Shr)),
                    _ => None,
                };
                if let Some(tok) = two {
                    self.idx += 2;
                    out.push(tok);
                    continue;
                }
            }

            match ch {
                '(' => {
                    self.bump();
                    out.push(Tok::LParen);
                }
                ')' => {
                    self.bump();
                    out.push(Tok::RParen);
                }
                ',' => {
                    self.bump();
                    out.push(Tok::Comma);
                }
                '.' => {
                    self.bump();
                    out.push(Tok::Dot);
                }
                '&' => {
                    self.bump();
                    out.push(Tok::Amp);
                }
                '+' => {
                    self.bump();
                    out.push(Tok::Op(BinOp::Add));
                }
                '-' => {
                    self.bump();
                    out.push(Tok::Op(BinOp::Sub));
                }
                '*' => {
                    self.bump();
                    out.push(Tok::Op(BinOp::Mul));
                }
                '/' => {
                    self.bump();
                    out.push(Tok::Op(BinOp::Div));
                }
                '%' => {
                    self.bump();
                    out.push(Tok::Op(BinOp::Mod));
                }
                '>' => {
                    self.bump();
                    out.push(Tok::Op(BinOp::Gt));
                }
                '<' => {
                    self.bump();
                    out.push(Tok::Op(BinOp::Lt));
                }
                '@' => {
                    self.bump();
                    let rest = self.take_while(|c| c.is_ascii_hexdigit() || c == 'x' || c == 'X');
                    if rest.is_empty() {
                        return None;
                    }
                    out.push(Tok::Address(format!("@{}", rest)));
                }
                c if c.is_ascii_digit() => {
                    let lit = self.take_while(|x| x.is_ascii_alphanumeric() || x == '_');
                    out.push(Tok::Number(lit));
                }
                c if c.is_ascii_alphabetic() || c == '_' => {
                    let ident = self.take_while(|x| x.is_ascii_alphanumeric() || x == '_');
                    match ident.as_str() {
                        "true" => out.push(Tok::True),
                        "false" => out.push(Tok::False),
                        "mut" => out.push(Tok::KeywordMut),
                        "as" => out.push(Tok::KeywordAs),
                        _ => out.push(Tok::Ident(ident)),
                    }
                }
                _ => return None,
            }
        }
        Some(out)
    }
}

struct Parser {
    toks: Vec<Tok>,
    idx: usize,
}

impl Parser {
    fn new(toks: Vec<Tok>) -> Self {
        Self { toks, idx: 0 }
    }

    fn peek(&self) -> Option<&Tok> {
        self.toks.get(self.idx)
    }

    fn bump(&mut self) -> Option<Tok> {
        let tok = self.toks.get(self.idx)?.clone();
        self.idx += 1;
        Some(tok)
    }

    fn consume(&mut self, tok: &Tok) -> bool {
        if self.peek() == Some(tok) {
            self.idx += 1;
            true
        } else {
            false
        }
    }

    fn parse(mut self) -> Option<Expr> {
        let expr = self.parse_expr_bp(0)?;
        if self.peek().is_some() {
            return None;
        }
        Some(expr)
    }

    fn parse_expr_bp(&mut self, min_bp: u8) -> Option<Expr> {
        let mut lhs = self.parse_prefix()?;
        lhs = self.parse_postfix(lhs)?;

        loop {
            let op = match self.peek() {
                Some(Tok::Op(op)) => *op,
                _ => break,
            };
            let (lbp, rbp) = op.binding_power();
            if lbp < min_bp {
                break;
            }
            self.bump();
            let mut rhs = self.parse_expr_bp(rbp)?;
            rhs = self.parse_postfix(rhs)?;
            lhs = Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }

        Some(lhs)
    }

    fn parse_prefix(&mut self) -> Option<Expr> {
        match self.bump()? {
            Tok::Amp => {
                let mutable = self.consume(&Tok::KeywordMut);
                let inner = self.parse_expr_bp(11)?;
                Some(Expr::Ref {
                    mutable,
                    expr: Box::new(inner),
                })
            }
            Tok::LParen => {
                let e = self.parse_expr_bp(0)?;
                if !self.consume(&Tok::RParen) {
                    return None;
                }
                Some(e)
            }
            Tok::Ident(id) => {
                let mut path = id;
                while self.consume(&Tok::ColonColon) {
                    let seg = match self.bump()? {
                        Tok::Ident(s) => s,
                        _ => return None,
                    };
                    path.push_str("::");
                    path.push_str(&seg);
                }
                Some(Expr::Path(path))
            }
            Tok::Number(n) => Some(Expr::Number(n)),
            Tok::Address(a) => Some(Expr::Address(a)),
            Tok::True => Some(Expr::Bool(true)),
            Tok::False => Some(Expr::Bool(false)),
            _ => None,
        }
    }

    fn parse_postfix(&mut self, mut expr: Expr) -> Option<Expr> {
        loop {
            match self.peek() {
                Some(Tok::Dot) => {
                    self.bump();
                    let name = match self.bump()? {
                        Tok::Ident(s) => s,
                        _ => return None,
                    };
                    if self.consume(&Tok::LParen) {
                        let args = self.parse_call_args()?;
                        let mut all = Vec::with_capacity(1 + args.len());
                        all.push(expr);
                        all.extend(args);
                        expr = Expr::Call {
                            func: name,
                            args: all,
                        };
                    } else {
                        expr = Expr::Field {
                            base: Box::new(expr),
                            field: name,
                        };
                    }
                }
                Some(Tok::LParen) => {
                    self.bump();
                    let args = self.parse_call_args()?;
                    let func = match expr {
                        Expr::Path(p) => p,
                        _ => return None,
                    };
                    expr = Expr::Call { func, args };
                }
                Some(Tok::KeywordAs) => {
                    self.bump();
                    let ty = self.parse_type_path()?;
                    expr = Expr::Cast {
                        expr: Box::new(expr),
                        ty,
                    };
                }
                _ => break,
            }
        }
        Some(expr)
    }

    fn parse_call_args(&mut self) -> Option<Vec<Expr>> {
        let mut out = Vec::new();
        if self.consume(&Tok::RParen) {
            return Some(out);
        }
        loop {
            let arg = self.parse_expr_bp(0)?;
            out.push(arg);
            if self.consume(&Tok::RParen) {
                break;
            }
            if !self.consume(&Tok::Comma) {
                return None;
            }
        }
        Some(out)
    }

    fn parse_type_path(&mut self) -> Option<String> {
        let mut out = String::new();
        let first = match self.bump()? {
            Tok::Ident(s) => s,
            _ => return None,
        };
        out.push_str(&first);
        while self.consume(&Tok::ColonColon) {
            out.push_str("::");
            let seg = match self.bump()? {
                Tok::Ident(s) => s,
                _ => return None,
            };
            out.push_str(&seg);
        }
        Some(out)
    }
}

pub(super) fn parse_expr(input: &str) -> Option<Expr> {
    let toks = Lexer::new(input).lex()?;
    Parser::new(toks).parse()
}

pub(super) fn strip_wrappers<'a>(expr: &'a Expr) -> &'a Expr {
    match expr {
        Expr::Ref { expr, .. } => strip_wrappers(expr),
        Expr::Cast { expr, ty } => {
            let _ = ty;
            strip_wrappers(expr)
        }
        _ => expr,
    }
}

pub(super) fn as_ident(expr: &Expr) -> Option<String> {
    match strip_wrappers(expr) {
        Expr::Path(p) if !p.contains("::") => Some(p.clone()),
        _ => None,
    }
}

pub(super) fn as_field_ident(expr: &Expr) -> Option<(String, String)> {
    match strip_wrappers(expr) {
        Expr::Field { base, field } => {
            let base_id = as_ident(base)?;
            Some((base_id, field.clone()))
        }
        _ => None,
    }
}

pub(super) fn as_call(expr: &Expr) -> Option<(String, Vec<Expr>)> {
    match strip_wrappers(expr) {
        Expr::Call { func, args } => Some((func.clone(), args.clone())),
        _ => None,
    }
}

pub(super) fn as_comparison(expr: &Expr) -> Option<(Expr, BinOp, Expr)> {
    match strip_wrappers(expr) {
        Expr::Binary { op, lhs, rhs } if op.is_comparison() => {
            Some((*lhs.clone(), *op, *rhs.clone()))
        }
        _ => None,
    }
}

pub(super) fn render_expr(expr: &Expr) -> String {
    match strip_wrappers(expr) {
        Expr::Path(p) => p.clone(),
        Expr::Number(n) => n.clone(),
        Expr::Bool(b) => {
            if *b {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
        Expr::Address(a) => a.clone(),
        Expr::Ref { mutable, expr } => {
            if *mutable {
                format!("&mut {}", render_expr(expr))
            } else {
                format!("&{}", render_expr(expr))
            }
        }
        Expr::Field { base, field } => format!("{}.{}", render_expr(base), field),
        Expr::Call { func, args } => {
            let rendered = args.iter().map(render_expr).collect::<Vec<_>>().join(", ");
            format!("{}({})", func, rendered)
        }
        Expr::Cast { expr, ty } => format!("({} as {})", render_expr(expr), ty),
        Expr::Binary { op, lhs, rhs } => {
            format!("{} {} {}", render_expr(lhs), op.as_str(), render_expr(rhs))
        }
    }
}
