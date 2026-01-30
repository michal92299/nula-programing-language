use anyhow::{Context, Result};
use clap::Parser;
use indextree::{Arena, NodeId};
use log::{debug, error, info};
use miette::{Diagnostic, NamedSource, SourceSpan};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use thiserror::Error;
use logos::Logos;
use winnow::prelude::*;
use winnow::token::*;
use winnow::combinator::*;

// Custom error types with miette integration
#[derive(Error, Diagnostic, Debug)]
enum FrontendError {
    #[error("IO error: {0}")]
    Io(#[from] io::Error),

    #[error("JSON serialization error: {0}")]
    Json(#[from] serde_json::Error),

    #[error("Lexing failed: {message}")]
    #[diagnostic(code(nula::lex_error))]
    Lex {
        message: String,
        #[source_code]
        src: NamedSource<String>,
        #[label("here")]
        span: SourceSpan,
    },

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

// Lexer tokens using Logos
#[derive(Logos, Debug, PartialEq, Clone)]
enum Token {
    #[token("write")]
    Write,

    #[token("require")]
    Require,

    #[regex("--[^\n]*", logos::skip)]  // Single-line comment
    Comment,

    #[regex("---[ \t]*\n(?s).*?\n---", logos::skip)]  // Multi-line comment
    MultiComment,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token(";")]
    Semi,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,

    #[regex(r#"[0-9]+"#)]
    Number,

    #[regex(r#"/[^/]+/[a-z]*/?"#)]  // Simplified path for require
    Path,

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

// AST Node representation using indextree
#[derive(Debug, Clone, Serialize, Deserialize)]
enum AstNode {
    Program(Vec<NodeId>),  // Root with statements
    Statement(StatementKind),
    Expression(ExpressionKind),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum StatementKind {
    Write(NodeId),  // write expression;
    Require(String), // require [path];
}

#[derive(Debug, Clone, Serialize, Deserialize)]
enum ExpressionKind {
    Literal(String),
    Ident(String),
    // Add more as needed
}

// Output to compiler (JSON)
#[derive(Debug, Serialize, Deserialize)]
struct FrontendOutput {
    source: String,
    ast_arena: Vec<AstNode>, // Serialized nodes
    root_id: usize,
}

// Serialize arena to vec (simplified: assumes nodes are in order)
fn serialize_arena(arena: &Arena<AstNode>) -> (Vec<AstNode>, NodeId) {
    let root = arena.iter().next().unwrap().0; // Assume first is root
    let nodes: Vec<_> = arena.iter().map(|(_, node)| node.data.clone()).collect();
    (nodes, root)
}

// Parser using winnow
fn parse_program(input: &mut &str) -> PResult<Vec<AstNode>> {
    terminated(many0(parse_statement), eof).parse_next(input)
}

fn parse_statement(input: &mut &str) -> PResult<AstNode> {
    alt((
        parse_write.map(AstNode::Statement),
        parse_require.map(AstNode::Statement),
    )).parse_next(input)
}

fn parse_write(input: &mut &str) -> PResult<StatementKind> {
    let _ = take_while(0.., |c: char| c.is_whitespace()).parse_next(input)?;
    let _ = literal("write").parse_next(input)?;
    let _ = take_while(1.., |c: char| c.is_whitespace()).parse_next(input)?;
    let expr = parse_expression.parse_next(input)?;
    let _ = literal(";").parse_next(input)?;
    Ok(StatementKind::Write(expr)) // Note: this would need to be NodeId, but simplified
    // In full, we'd build the arena here
}

fn parse_require(input: &mut &str) -> PResult<StatementKind> {
    let _ = take_while(0.., |c: char| c.is_whitespace()).parse_next(input)?;
    let _ = literal("require").parse_next(input)?;
    let _ = take_while(1.., |c: char| c.is_whitespace()).parse_next(input)?;
    let path = delimited(literal("["), take_while(1.., |c| c != ']'), literal("]")).parse_next(input)?;
    let _ = literal(";").parse_next(input)?;
    Ok(StatementKind::Require(path.to_string()))
}

fn parse_expression(input: &mut &str) -> PResult<NodeId> {
    // Placeholder: parse literal or ident
    // For now, assume literal as [content]
    let content = delimited(literal("["), take_while(0.., |c| c != ']'), literal("]")).parse_next(input)?;
    // In full: create node in arena
    // Here, simplified
    Err(winnow::error::ErrMode::Cut(winnow::error::ContextError::new())) // Placeholder
}

// Actually, we need to build the arena during parsing
struct Parser<'a> {
    source: &'a str,
    arena: Arena<AstNode>,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            arena: Arena::new(),
        }
    }

    fn parse(&mut self) -> Result<NodeId> {
        let mut input = self.source;
        let statements = many0(|i: &mut _| self.parse_statement(i)).parse(&mut input).map_err(|e| {
            FrontendError::Parse {
                message: e.to_string(),
                src: NamedSource::new("input.nula", self.source.to_string()),
                span: SourceSpan::new(0.into(), 0.into()), // Placeholder span
            }
        })?;
        let root = self.arena.new_node(AstNode::Program(statements));
        Ok(root)
    }

    fn parse_statement(&mut self, input: &mut &'a str) -> PResult<NodeId> {
        alt((
            self.parse_write,
            self.parse_require,
        )).parse_next(input)
    }

    fn parse_write(&mut self, input: &mut &'a str) -> PResult<NodeId> {
        let start = input.as_ptr() as usize - self.source.as_ptr() as usize;
        let _ = take_while(0.., char::is_whitespace).parse_next(input)?;
        let _ = "write".parse_next(input)?;
        let _ = take_while(1.., char::is_whitespace).parse_next(input)?;
        let expr_id = self.parse_expression(input)?;
        let _ = ";".parse_next(input)?;
        let end = input.as_ptr() as usize - self.source.as_ptr() as usize;
        let stmt = self.arena.new_node(AstNode::Statement(StatementKind::Write(expr_id)));
        Ok(stmt)
    }

    fn parse_require(&mut self, input: &mut &'a str) -> PResult<NodeId> {
        let _ = take_while(0.., char::is_whitespace).parse_next(input)?;
        let _ = "require".parse_next(input)?;
        let _ = take_while(1.., char::is_whitespace).parse_next(input)?;
        let path = delimited("[", take_while(1.., |c| c != ']'), "]").parse_next(input)?.to_string();
        let _ = ";".parse_next(input)?;
        let stmt = self.arena.new_node(AstNode::Statement(StatementKind::Require(path)));
        Ok(stmt)
    }

    fn parse_expression(&mut self, input: &mut &'a str) -> PResult<NodeId> {
        // Simple literal [content]
        let content = delimited("[", take_while(0.., |c| c != ']'), "]").parse_next(input)?.to_string();
        let expr = self.arena.new_node(AstNode::Expression(ExpressionKind::Literal(content)));
        Ok(expr)
    }
}

// CLI
#[derive(Parser, Debug)]
#[command(version, about = "Nula Frontend (Lexer & Parser)")]
struct Cli {
    /// Input source file
    #[arg(short, long)]
    input: PathBuf,

    /// Output JSON file
    #[arg(short, long)]
    output: PathBuf,
}

fn lex_source(source: &str) -> Result<Vec<Token>> {
    let mut lexer = Token::lexer(source);
    let mut tokens = Vec::new();
    while let Some(token) = lexer.next() {
        if token == Token::Error {
            return Err(FrontendError::Lex {
                message: "Invalid token".to_string(),
                src: NamedSource::new("input.nula", source.to_string()),
                span: SourceSpan::new(lexer.span().start.into(), (lexer.span().end - lexer.span().start).into()),
            }.into());
        }
        tokens.push(token);
    }
    Ok(tokens)
}

fn main() -> Result<()> {
    env_logger::init();

    let cli = Cli::parse();

    // Read input source
    let mut input_file = File::open(&cli.input).context("Failed to open input file")?;
    let mut source = String::new();
    input_file.read_to_string(&mut source)?;

    // Lex (though parser uses string directly, but for validation)
    let _tokens = lex_source(&source)?; // Can integrate Logos with winnow if needed

    // Parse
    let mut parser = Parser::new(&source);
    let root = parser.parse()?;

    // Serialize
    let (ast_arena, _) = serialize_arena(&parser.arena); // Note: root_id is root.index()
    let output = FrontendOutput {
        source,
        ast_arena,
        root_id: root.index(),
    };
    let json = serde_json::to_string(&output)?;

    // Write output JSON
    fs::write(&cli.output, json).context("Failed to write output JSON")?;

    info!("Frontend processing successful!");
    Ok(())
}
