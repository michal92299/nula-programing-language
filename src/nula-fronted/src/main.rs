use anyhow::{Context, Result};
use clap::Parser as ClapParser;
use indextree::{Arena, NodeId};
use miette::{Diagnostic, NamedSource, SourceSpan};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs::{self, File};
use std::io::Read;
use std::path::PathBuf;
use std::marker::PhantomData;
use thiserror::Error;
use logos::{Logos, Lexer};
use winnow::prelude::*;
use winnow::token::{one_of, take_while};
use winnow::combinator::*;
use winnow::Parser;
use winnow::error::{ContextError, ErrMode};

#[derive(Error, Diagnostic, Debug)]
enum FrontendError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("JSON serialization error: {0}")]
    Json(#[from] serde_json::Error),

    #[error("Parsing failed: {message}")]
    #[diagnostic(code(nula::parse_error))]
    Parse {
        message: String,
        #[source_code]
        src: NamedSource<String>,
        #[label("here")]
        span: SourceSpan,
    },
}

#[derive(Logos, Debug, PartialEq, Clone)]
enum Token {
    #[token("write")]
    Write,
    #[token("require")]
    Require,
    #[token("def")]
    Def,
    #[token("end")]
    End,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    
    #[regex("--[^\n]*", logos::skip)]
    Comment,
    #[regex("---", parse_multiline_comment)]
    MultiComment,
    
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("=")]
    Assign,
    #[token(",")]
    Comma,
    
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,
    #[regex("[0-9]+")]
    Integer,
}

fn parse_multiline_comment(lex: &mut Lexer<Token>) -> logos::Skip {
    let remainder = lex.remainder();
    if let Some(end_idx) = remainder.find("---") {
        lex.bump(end_idx + 3);
        logos::Skip
    } else {
        lex.bump(remainder.len());
        logos::Skip
    }
}

// --- AST Definitions ---

#[derive(Debug, Clone, Serialize, Deserialize)]
enum AstNode<T> {
    Program(Vec<T>),
    Statement(StatementKind<T>),
    Expression(ExpressionKind<T>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum StatementKind<T> {
    Require(String),
    Write(T),
    Assignment { name: String, value: T },
    FunctionDef { name: String, args: Vec<String>, body: Vec<T> },
    If { condition: T, then_block: Vec<T>, else_block: Option<Vec<T>> },
    ExprStmt(T),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum ExpressionKind<T> {
    Literal(String),
    Integer(i64),
    Variable(String),
    Call { name: String, args: Vec<T> },
    #[serde(skip)]
    _Marker(PhantomData<T>),
}

impl<T> ExpressionKind<T> {
    fn literal(s: String) -> Self { ExpressionKind::Literal(s) }
    fn integer(i: i64) -> Self { ExpressionKind::Integer(i) }
    fn variable(s: String) -> Self { ExpressionKind::Variable(s) }
}

#[derive(Debug, Serialize, Deserialize)]
struct FrontendOutput {
    source: String,
    ast_nodes: Vec<AstNode<usize>>,
    root_index: usize,
}

// --- Parser ---

struct NulaParser<'a> {
    arena: Arena<AstNode<NodeId>>,
    source: &'a str,
}

impl<'a> NulaParser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            arena: Arena::new(),
            source,
        }
    }

    fn parse(&mut self) -> Result<NodeId, FrontendError> {
        let mut input = self.source;
        let statements = self.parse_block(&mut input)?;

        // Ensure we consumed everything (except trailing whitespace)
        let _: Result<&str, ErrMode<ContextError>> = take_while(0.., |c: char| c.is_whitespace()).parse_next(&mut input);

        if !input.is_empty() {
             return Err(FrontendError::Parse {
                message: format!("Unexpected input remaining: {:.20}...", input),
                src: NamedSource::new("input.nula", self.source.to_string()),
                span: SourceSpan::new(0usize.into(), 0usize.into()),
            });
        }

        let root = self.arena.new_node(AstNode::Program(statements));
        Ok(root)
    }

    fn parse_block(&mut self, input: &mut &'a str) -> Result<Vec<NodeId>, FrontendError> {
        let mut statements = Vec::new();
        loop {
            // Consume whitespace
            let _: Result<&str, ErrMode<ContextError>> = take_while(0.., |c: char| c.is_whitespace()).parse_next(input);

            if input.is_empty() {
                break;
            }
            
            // Check terminators for blocks using peek with explicit types
            if peek::<_, _, ContextError, _>("end").parse_next(input).is_ok() {
                break;
            }
            if peek::<_, _, ContextError, _>("else").parse_next(input).is_ok() {
                break;
            }

            match self.parse_statement(input) {
                Ok(stmt) => statements.push(stmt),
                Err(_) => {
                     return Err(FrontendError::Parse {
                        message: "Expected statement".to_string(),
                        src: NamedSource::new("input.nula", self.source.to_string()),
                        span: SourceSpan::new(0usize.into(), 0usize.into()),
                    });
                }
            }
        }
        Ok(statements)
    }

    fn parse_statement(&mut self, input: &mut &'a str) -> ModalResult<NodeId> {
        // Dispatch based on keyword or identifier
        if peek::<_, _, ContextError, _>("write").parse_next(input).is_ok() {
            self.parse_write(input)
        } else if peek::<_, _, ContextError, _>("require").parse_next(input).is_ok() {
            self.parse_require(input)
        } else if peek::<_, _, ContextError, _>("def").parse_next(input).is_ok() {
            self.parse_def(input)
        } else if peek::<_, _, ContextError, _>("if").parse_next(input).is_ok() {
            self.parse_if(input)
        } else {
            // Assignment (Ident =) or Expression
            // Lookahead for assignment
            let checkpoint = *input;
            if self.parse_assignment_lookahead(input) {
                *input = checkpoint; // Backtrack to parse properly
                self.parse_assignment(input)
            } else {
                *input = checkpoint;
                // Fallback to expression statement
                let expr = self.parse_expression(input)?;
                Ok(self.arena.new_node(AstNode::Statement(StatementKind::ExprStmt(expr))))
            }
        }
    }

    fn parse_assignment_lookahead(&self, mut input: &str) -> bool {
        // Check if pattern is Ident = ...
        // We use a tuple parser with explicit types to ensure proper inference
        let check: ModalResult<_, ContextError> = (
            take_while(0.., |c: char| c.is_whitespace()),
            take_while(1.., |c: char| c.is_alphanumeric() || c == '_'),
            take_while(0.., |c: char| c.is_whitespace()),
            "=",
        ).parse_next(&mut input);
        
        check.is_ok()
    }

    fn parse_assignment(&mut self, input: &mut &'a str) -> ModalResult<NodeId> {
        let name = self.parse_ident(input)?;
        let _ = take_while(0.., |c: char| c.is_whitespace()).parse_next(input)?;
        let _ = "=".parse_next(input)?;
        let _ = take_while(0.., |c: char| c.is_whitespace()).parse_next(input)?;
        let value = self.parse_expression(input)?;
        
        Ok(self.arena.new_node(AstNode::Statement(StatementKind::Assignment { name: name.to_string(), value })))
    }

    fn parse_require(&mut self, input: &mut &'a str) -> ModalResult<NodeId> {
        let _ = "require".parse_next(input)?;
        let _ = take_while(1.., |c: char| c.is_whitespace()).parse_next(input)?;
        let path_content = self.parse_string_bracket(input)?;
        Ok(self.arena.new_node(AstNode::Statement(StatementKind::Require(path_content))))
    }

    fn parse_write(&mut self, input: &mut &'a str) -> ModalResult<NodeId> {
        let _ = "write".parse_next(input)?;
        let _ = take_while(1.., |c: char| c.is_whitespace()).parse_next(input)?;
        let expr = self.parse_expression(input)?;
        Ok(self.arena.new_node(AstNode::Statement(StatementKind::Write(expr))))
    }

    fn parse_def(&mut self, input: &mut &'a str) -> ModalResult<NodeId> {
        let _ = "def".parse_next(input)?;
        let _ = take_while(1.., |c: char| c.is_whitespace()).parse_next(input)?;
        let name = self.parse_ident(input)?.to_string();
        let _ = take_while(0.., |c: char| c.is_whitespace()).parse_next(input)?;
        
        let mut args = Vec::new();
        // Use opt to check for existence of opening parenthesis
        if opt::<_, _, ContextError, _>("(").parse_next(input)?.is_some() {
            // Parse args
            loop {
                 let _ = take_while(0.., |c: char| c.is_whitespace()).parse_next(input)?;
                 if peek::<_, _, ContextError, _>(")").parse_next(input).is_ok() {
                     break;
                 }
                 let arg = self.parse_ident(input)?;
                 args.push(arg.to_string());
                 let _ = take_while(0.., |c: char| c.is_whitespace()).parse_next(input)?;
                 // Optional comma
                 if opt::<_, _, ContextError, _>(",").parse_next(input)?.is_none() {
                     break;
                 }
            }
            let _ = ")".parse_next(input)?;
        }

        // Body
        let body_nodes = self.parse_block(input).map_err(|_| winnow::error::ErrMode::Backtrack(ContextError::new()))?;
        
        let _ = take_while(0.., |c: char| c.is_whitespace()).parse_next(input)?;
        let _ = "end".parse_next(input)?;

        Ok(self.arena.new_node(AstNode::Statement(StatementKind::FunctionDef { name, args, body: body_nodes })))
    }

    fn parse_if(&mut self, input: &mut &'a str) -> ModalResult<NodeId> {
        let _ = "if".parse_next(input)?;
        let _ = take_while(1.., |c: char| c.is_whitespace()).parse_next(input)?;
        let condition = self.parse_expression(input)?;
        
        let then_block = self.parse_block(input).map_err(|_| winnow::error::ErrMode::Backtrack(ContextError::new()))?;
        
        let _ = take_while(0.., |c: char| c.is_whitespace()).parse_next(input)?;
        
        let mut else_block = None;
        if opt::<_, _, ContextError, _>("else").parse_next(input)?.is_some() {
            let block = self.parse_block(input).map_err(|_| winnow::error::ErrMode::Backtrack(ContextError::new()))?;
            else_block = Some(block);
            let _ = take_while(0.., |c: char| c.is_whitespace()).parse_next(input)?;
        }
        
        let _ = "end".parse_next(input)?;

        Ok(self.arena.new_node(AstNode::Statement(StatementKind::If { condition, then_block, else_block })))
    }

    fn parse_expression(&mut self, input: &mut &'a str) -> ModalResult<NodeId> {
        self.parse_atom(input)
    }

    fn parse_atom(&mut self, input: &mut &'a str) -> ModalResult<NodeId> {
        let _ = take_while(0.., |c: char| c.is_whitespace()).parse_next(input)?;

        if peek::<_, _, ContextError, _>("[").parse_next(input).is_ok() {
            let s = self.parse_string_bracket(input)?;
            Ok(self.arena.new_node(AstNode::Expression(ExpressionKind::literal(s))))
        } else if peek::<_, _, ContextError, _>(one_of::<_, _, ContextError>(|c: char| c.is_ascii_digit())).parse_next(input).is_ok() {
             let num_str: &str = take_while(1.., |c: char| c.is_ascii_digit()).parse_next(input)?;
             let num: i64 = num_str.parse().unwrap_or(0);
             Ok(self.arena.new_node(AstNode::Expression(ExpressionKind::integer(num))))
        } else {
            // Ident or Call
            let ident = self.parse_ident(input)?;
            let _ = take_while(0.., |c: char| c.is_whitespace()).parse_next(input)?;
            if peek::<_, _, ContextError, _>("(").parse_next(input).is_ok() {
                // Call
                let _ = "(".parse_next(input)?;
                let mut args = Vec::new();
                 loop {
                     let _ = take_while(0.., |c: char| c.is_whitespace()).parse_next(input)?;
                     if peek::<_, _, ContextError, _>(")").parse_next(input).is_ok() {
                         break;
                     }
                     let arg = self.parse_expression(input)?;
                     args.push(arg);
                     let _ = take_while(0.., |c: char| c.is_whitespace()).parse_next(input)?;
                     if opt::<_, _, ContextError, _>(",").parse_next(input)?.is_none() {
                         break;
                     }
                }
                let _ = ")".parse_next(input)?;
                Ok(self.arena.new_node(AstNode::Expression(ExpressionKind::Call { name: ident.to_string(), args })))
            } else {
                // Var
                Ok(self.arena.new_node(AstNode::Expression(ExpressionKind::variable(ident.to_string()))))
            }
        }
    }

    fn parse_ident(&mut self, input: &mut &'a str) -> ModalResult<&'a str> {
        take_while(1.., |c: char| c.is_alphanumeric() || c == '_').parse_next(input)
    }

    fn parse_string_bracket(&mut self, input: &mut &'a str) -> ModalResult<String> {
        let _ = "[".parse_next(input)?;
        let content: Vec<String> = repeat(0.., alt((
            take_while(1.., |c| c != '[' && c != ']').map(String::from),
            |i: &mut &'a str| {
                 let nested = self.parse_string_bracket(i)?;
                 Ok(format!("[{}]", nested))
            }
        ))).parse_next(input)?;
        let _ = "]".parse_next(input)?;
        Ok(content.join(""))
    }
}

#[derive(ClapParser, Debug)]
#[command(version, about = "Nula Frontend (Lexer & Parser)")]
struct Cli {
    #[arg(short, long)]
    input: PathBuf,
    #[arg(short, long)]
    output: PathBuf,
}

fn main() -> Result<()> {
    env_logger::init();
    let cli = Cli::parse();

    let mut input_file = File::open(&cli.input).context("Failed to open input file")?;
    let mut source = String::new();
    input_file.read_to_string(&mut source)?;

    // Basic Lexer Check (Logos)
    let mut lexer = Token::lexer(&source);
    while let Some(token) = lexer.next() {
        if let Err(_) = token { /* Handle lex error */ }
    }

    // Parser (Winnow)
    let mut parser = NulaParser::new(&source);
    let root_id = parser.parse()?;

    // Flatten Arena
    let mut flat_nodes: Vec<AstNode<usize>> = Vec::new();
    let mut id_map: HashMap<NodeId, usize> = HashMap::new();

    let mut node_ids = Vec::new();
    
    // Use descendants to gather all reachable nodes from root
    for id in root_id.descendants(&parser.arena) {
        if !id_map.contains_key(&id) {
            id_map.insert(id, flat_nodes.len());
            flat_nodes.push(AstNode::Program(vec![])); // Placeholder
            node_ids.push(id);
        }
    }
    if !id_map.contains_key(&root_id) {
        id_map.insert(root_id, flat_nodes.len());
        flat_nodes.push(AstNode::Program(vec![]));
        node_ids.push(root_id);
    }

    // Helper closure to map ID to usize
    let mapper = |id: &NodeId| *id_map.get(id).unwrap_or(&0);
    let map_vec = |v: &Vec<NodeId>| v.iter().map(mapper).collect();

    for (i, &old_id) in node_ids.iter().enumerate() {
        let node = parser.arena.get(old_id).unwrap().get();
        let new_node = match node {
            AstNode::Program(stmts) => AstNode::Program(map_vec(stmts)),
            AstNode::Statement(kind) => match kind {
                StatementKind::Write(expr_id) => AstNode::Statement(StatementKind::Write(mapper(expr_id))),
                StatementKind::Require(s) => AstNode::Statement(StatementKind::Require(s.clone())),
                StatementKind::Assignment { name, value } => AstNode::Statement(StatementKind::Assignment { name: name.clone(), value: mapper(value) }),
                StatementKind::ExprStmt(val) => AstNode::Statement(StatementKind::ExprStmt(mapper(val))),
                StatementKind::FunctionDef { name, args, body } => AstNode::Statement(StatementKind::FunctionDef { name: name.clone(), args: args.clone(), body: map_vec(body) }),
                StatementKind::If { condition, then_block, else_block } => AstNode::Statement(StatementKind::If { 
                    condition: mapper(condition), 
                    then_block: map_vec(then_block), 
                    else_block: else_block.as_ref().map(map_vec) 
                }),
            },
            AstNode::Expression(kind) => match kind {
                ExpressionKind::Literal(s) => AstNode::Expression(ExpressionKind::Literal(s.clone())),
                ExpressionKind::Integer(i) => AstNode::Expression(ExpressionKind::Integer(*i)),
                ExpressionKind::Variable(s) => AstNode::Expression(ExpressionKind::Variable(s.clone())),
                ExpressionKind::Call { name, args } => AstNode::Expression(ExpressionKind::Call { name: name.clone(), args: map_vec(args) }),
                ExpressionKind::_Marker(_) => AstNode::Expression(ExpressionKind::_Marker(PhantomData)),
            },
        };
        flat_nodes[i] = new_node;
    }

    let output = FrontendOutput {
        source,
        ast_nodes: flat_nodes,
        root_index: *id_map.get(&root_id).unwrap_or(&0),
    };
    
    let json = serde_json::to_string(&output)?;
    fs::write(&cli.output, json).context("Failed to write output JSON")?;

    Ok(())
}
