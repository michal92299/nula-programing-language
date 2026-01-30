use anyhow::{Context, Result};
use clap::Parser;
use cranelift::prelude::*;
use cranelift_codegen::isa::lookup as isa_lookup;
use cranelift_codegen::settings::{self, Configurable};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use log::{info, error, debug};
use miette::{Diagnostic, NamedSource, SourceSpan};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs::{self, File};
use std::io::{Read};
use std::path::{PathBuf};
use std::str::FromStr;
use target_lexicon::Triple;
use thiserror::Error;

// Mock Tycho module for Semantic Analysis
mod tycho {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct TypeInfo {
        pub name: String,
        pub size: u32,
    }

    pub struct Analyzer;

    impl Analyzer {
        pub fn analyze(_nodes: &[AstNode<usize>]) -> Result<HashMap<usize, TypeInfo>> {
            Ok(HashMap::new())
        }
    }
}

#[derive(Error, Diagnostic, Debug)]
enum CompilerError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("JSON parsing error: {0}")]
    Json(#[from] serde_json::Error),

    #[error("Config error: {0}")]
    Config(String),

    #[error("Cranelift module error: {0}")]
    Module(#[from] cranelift_module::ModuleError),

    #[error("Source error")]
    #[diagnostic(code(nula::source_error))]
    SourceError {
        #[source_code]
        src: NamedSource<String>,
        #[label("here")]
        span: SourceSpan,
    }
}

// AST Definitions matching Frontend
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
    _Marker(std::marker::PhantomData<T>),
}

#[derive(Debug, Serialize, Deserialize)]
struct FrontendOutput {
    source: String,
    ast_nodes: Vec<AstNode<usize>>,
    root_index: usize,
}

#[derive(Parser, Debug)]
#[command(version, about = "Nula Compiler Backend")]
struct Cli {
    #[arg(short, long)]
    input: PathBuf,
    #[arg(short, long)]
    output: PathBuf,
    #[arg(long, default_value = "native")]
    target: String,
    #[arg(long)]
    arena_allocator: bool,
    #[arg(long)]
    manual_memory: bool,
    #[arg(long)]
    static_binary: bool,
    #[arg(long)]
    dynamic_binary: bool,
    #[arg(long)]
    small_binary: bool,
    #[arg(long)]
    fast_compile: bool,
}

struct NulaCompiler {
    nodes: Vec<AstNode<usize>>,
    root_index: usize,
    module: ObjectModule,
    cli: Cli,
}

impl NulaCompiler {
    fn new(frontend_output: FrontendOutput, cli: Cli) -> Result<Self> {
        let nodes = frontend_output.ast_nodes;
        let root_index = frontend_output.root_index;

        let triple = if cli.target == "native" {
            Triple::host()
        } else {
            Triple::from_str(&cli.target).map_err(|e| CompilerError::Config(e.to_string()))?
        };

        let mut flag_builder = settings::builder();
        if cli.small_binary {
            flag_builder.set("opt_level", "size").unwrap();
        } else if cli.fast_compile {
            flag_builder.set("opt_level", "speed").unwrap();
        } else {
            flag_builder.set("opt_level", "speed_and_size").unwrap();
        }

        if cli.arena_allocator {
            info!("Arena allocator enabled");
        }

        let isa_builder = isa_lookup(triple).map_err(|e| CompilerError::Config(e.to_string()))?;
        let isa = isa_builder.finish(settings::Flags::new(flag_builder)).unwrap();

        let builder = ObjectBuilder::new(isa, "nula_module", cranelift_module::default_libcall_names())?;
        let module = ObjectModule::new(builder);

        Ok(Self {
            nodes,
            root_index,
            module,
            cli,
        })
    }

    fn compile(mut self) -> Result<()> {
        let _types = tycho::Analyzer::analyze(&self.nodes)?;
        info!("Semantic analysis passed");

        let mut ctx = self.module.make_context();
        let mut func_builder_ctx = FunctionBuilderContext::new();

        let mut sig = self.module.make_signature();
        sig.returns.push(AbiParam::new(types::I32));
        ctx.func.signature = sig;

        let func_id = self.module.declare_function("main", Linkage::Export, &ctx.func.signature)?;

        {
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_builder_ctx);
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            if self.root_index < self.nodes.len() {
                self.codegen_node(self.root_index, &mut builder)?;
            }

            let ret_val = builder.ins().iconst(types::I32, 0);
            builder.ins().return_(&[ret_val]);

            builder.finalize();
        }

        self.module.define_function(func_id, &mut ctx)?;
        self.module.clear_context(&mut ctx);

        let product = self.module.finish();
        let obj_data = product.emit()?;
        fs::write(&self.cli.output, obj_data).context("Failed to write object file")?;

        Ok(())
    }

    fn codegen_node(&self, index: usize, builder: &mut FunctionBuilder) -> Result<()> {
        if let Some(node) = self.nodes.get(index) {
            match node {
                AstNode::Program(stmt_indices) => {
                    for &stmt_idx in stmt_indices {
                        self.codegen_node(stmt_idx, builder)?;
                    }
                }
                AstNode::Statement(stmt) => match stmt {
                    StatementKind::Write(expr_idx) => {
                        self.codegen_node(*expr_idx, builder)?;
                        debug!("Generated write");
                    }
                    StatementKind::Require(path) => {
                        debug!("Required: {}", path);
                    }
                    StatementKind::Assignment { name, value: _ } => {
                        debug!("Assignment to {}", name);
                        // Simplified: In a real implementation, would use stack slots
                    }
                    StatementKind::FunctionDef { name, .. } => {
                        debug!("Function definition: {}", name);
                    }
                    StatementKind::If { .. } => {
                        debug!("If Statement");
                    }
                    StatementKind::ExprStmt(expr_idx) => {
                        self.codegen_node(*expr_idx, builder)?;
                    }
                },
                AstNode::Expression(expr) => match expr {
                    ExpressionKind::Literal(text) => {
                        debug!("Literal: {}", text);
                    }
                    ExpressionKind::Integer(val) => {
                        debug!("Integer: {}", val);
                    }
                    ExpressionKind::Variable(name) => {
                        debug!("Variable usage: {}", name);
                    }
                    ExpressionKind::Call { name, .. } => {
                        debug!("Call: {}", name);
                    }
                    _ => {}
                },
            }
        }
        Ok(())
    }
}

fn main() -> Result<()> {
    env_logger::init();
    let cli = Cli::parse();

    let mut input_file = File::open(&cli.input).context("Failed to open input JSON")?;
    let mut json_str = String::new();
    input_file.read_to_string(&mut json_str)?;

    let frontend_output: FrontendOutput = serde_json::from_str(&json_str)?;
    let compiler = NulaCompiler::new(frontend_output, cli)?;

    if let Err(e) = compiler.compile() {
        match e.downcast::<CompilerError>() {
            Ok(diag) => {
                eprintln!("{:?}", miette::Report::new(diag));
                std::process::exit(1);
            }
            Err(e) => {
                error!("Compiler failed: {}", e);
                return Err(e);
            }
        }
    }

    info!("Compilation successful!");
    Ok(())
}
