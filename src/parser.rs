use pest::iterators::{Pair, Pairs};
use pest::Parser;
use regex::Regex; use core::panic;
use std::collections::HashMap;
// Import the Parser trait
use std::{default, fmt};
use std::error::Error;
use std::sync::Arc;

#[derive(Parser)]
#[grammar = "grammar.pest"] // Path to the Pest grammar file
pub struct MyParser;

// Further code to utilize MyParser for parsing the input
pub enum Expr {
    Int(i32),
    Var(String),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Cmp(Box<Expr>, Box<Expr>),
    Str(String),
    Bool(String),
    Func(Arc<dyn Fn(Vec<Expr>) -> Result<Expr, String> + Send + Sync>),
    Exec(String, Vec<Expr>),
    Symbol(String),
    Byte(Vec<u8>),
    Array(Vec<Expr>),
    ArrayAccess(String, Box<Expr>),
    External(String, Vec<String>, Vec<String>),
    BlockExpression(Vec<Stmt>, Box<Expr>),
    UserFunction(Vec<String>, Box<Expr>),
    Range(Box<Expr>, Box<Expr>),
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Expr::Int(a), Expr::Int(b)) => a == b,
            (Expr::Var(a), Expr::Var(b)) => a == b,
            (Expr::Add(a1, b1), Expr::Add(a2, b2)) => a1 == a2 && b1 == b2,
            (Expr::Sub(a1, b1), Expr::Sub(a2, b2)) => a1 == a2 && b1 == b2,
            (Expr::Str(a), Expr::Str(b)) => a == b,
            (Expr::Func(_), Expr::Func(_)) => false,
            (Expr::Exec(a1, a2), Expr::Exec(b1, b2)) => a1 == b1 && a2 == b2,
            _ => false,
        }
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Int(value) => f.debug_tuple("Int").field(value).finish(),
            Expr::Var(name) => f.debug_tuple("Var").field(name).finish(),
            Expr::Str(s) => f.debug_tuple("Str").field(s).finish(),
            Expr::Add(left, right) => f
                .debug_struct("Add")
                .field("left", left)
                .field("right", right)
                .finish(),
            Expr::Sub(left, right) => f
                .debug_struct("Sub")
                .field("left", left)
                .field("right", right)
                .finish(),
            Expr::Func(_) => f
                .debug_tuple("Func")
                .field(&"Box<dyn Fn() -> Result<Expr, String>>")
                .finish(),
            Expr::Exec(s, _) => f.debug_tuple("Exec").field(s).finish(),
            Expr::Symbol(s) => f.debug_tuple("Symbol").field(s).finish(),
            Expr::Byte(s) => f.debug_tuple("Byte").field(s).finish(),
            Expr::Array(s) => f.debug_tuple("Array").field(s).finish(),
            default => todo!()
        }
    }
}

impl Clone for Expr {
    fn clone(&self) -> Self {
        match self {
            Expr::Int(value) => Expr::Int(*value),
            Expr::Var(name) => Expr::Var(name.clone()),
            Expr::Str(s) => Expr::Str(s.clone()),
            Expr::Add(left, right) => Expr::Add(left.clone(), right.clone()),
            Expr::Sub(left, right) => Expr::Sub(left.clone(), right.clone()),
            Expr::Func(f) => Expr::Func(f.clone()),
            Expr::Exec(s, args) => {
                // Ensure both the command and the arguments are cloned properly
                Expr::Exec(s.clone(), args.iter().map(|arg| arg.clone()).collect())
            }
            Expr::External(a, b, c) => Expr::External(a.clone(),b.clone(), c.clone()),
            Expr::Symbol(s) => Expr::Symbol(s.clone()),
            Expr::Byte(s) => Expr::Byte(s.clone()),
            Expr::Array(s) => Expr::Array(s.clone()),
            Expr::Bool(b) => Expr::Bool(b.clone()),
            Expr::Cmp(left, right) => Expr::Cmp(left.clone(), right.clone()),
            Expr::UserFunction(s, b) => Expr::UserFunction(s.clone(), b.clone()),
            Expr::BlockExpression(s, b) => Expr::BlockExpression(s.clone(), b.clone()),
            Expr::ArrayAccess(a, b) => Expr::ArrayAccess(a.clone(), b.clone()),
            Expr::Range(a, b) => Expr::Range(a.clone(), b.clone()),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Assign(String, Expr),
    Print(Expr),
    Connect(String, u16, Expr), // IP, port, message
    Listen(String),
    External(String, String, String, Vec<String>, Vec<String>),
    If(Expr, Vec<Stmt>, Option<Box<Stmt>>),
    Block(Vec<Stmt>),
    Foreach(String, Expr, Vec<Stmt>),
    FunctionDeclaration(String, Expr),
}

#[derive(Debug, PartialEq)]
pub struct Program(pub Vec<Stmt>);

pub fn build_ast(mut pairs: Pairs<Rule>) -> Program {
    let mut statements = Vec::new();
    for pair in pairs.next().unwrap().into_inner() {
        match pair.as_rule() {
            Rule::expression_statement => {
                statements.push(parse_print_statement(pair.into_inner()));
            }
            Rule::let_statement => {
                statements.push(parse_let_statement(pair.into_inner()));
            }
            Rule::print_statement => {
                statements.push(parse_print_statement(pair.into_inner()));
            }
            Rule::connect_command => {
                statements.push(parse_connect_statement(pair.into_inner()));
            }
            Rule::listen_command => {
                statements.push(parse_listen_statement(pair.into_inner()));
            }
            Rule::external_definition => {
                statements.push(parse_external_statement(pair.into_inner()));
            }
            Rule::if_statement => {
                statements.push(parse_if_statement(pair.into_inner()).unwrap());
            }
            Rule::foreach_statement => {
                statements.push(parse_foreach_statement(pair.into_inner()));
            }
            Rule::function_declaration => {
                statements.push(parse_function_declaration(pair.into_inner()));
            }
            Rule::EOI => {
                break;
            }
            _ => unreachable!(),
        }
    }

    Program(statements)
}

fn parse_function_declaration(mut pairs: Pairs<Rule>) -> Stmt {

    let mut execute = pairs.next().unwrap().into_inner();

    let identifier = execute.next().unwrap().as_str().to_string();
    let args: Vec<String> = execute.map(|x| x.as_str().to_string()).collect();
    let block = parse_term(pairs.next().unwrap()).unwrap();

    let userfunction = Expr::UserFunction(args, Box::new(block));

    Stmt::FunctionDeclaration(identifier, userfunction)
}

fn parse_let_statement(mut pairs: Pairs<Rule>) -> Stmt {
    dbg!(&pairs);
    let identifier = pairs.next().unwrap().as_str().to_string();
    let expr = parse_expression(pairs.next().unwrap().into_inner());

    Stmt::Assign(identifier, expr.unwrap())
}

fn parse_print_statement(mut pairs: Pairs<Rule>) -> Stmt {
    let expr = parse_expression(pairs.next().unwrap().into_inner());
    Stmt::Print(expr.unwrap())
}

fn parse_connect_statement(mut pairs: Pairs<Rule>) -> Stmt {
    let ip = pairs.next().unwrap().as_str().to_string();
    let port = pairs.next().unwrap().as_str().parse::<u16>().unwrap(); // Simple error handling; improve in production

    if let Some(x) = pairs.next() {
        let message = x;
        Stmt::Connect(ip, port, parse_expression(message.into_inner()).unwrap())
    } else {
        Stmt::Connect(ip, port, Expr::Str("\n".to_string()))
    }
}

fn parse_listen_statement(mut pairs: Pairs<Rule>) -> Stmt {
    let port = pairs.next().unwrap().as_str().to_string();
    Stmt::Listen(port)
}

fn parse_foreach_statement(mut pairs: Pairs<Rule>) -> Stmt {
    let name = pairs.next().unwrap().as_str().to_string();
    let arr = parse_expression(pairs.next().unwrap().into_inner()).unwrap();

    let commands = pairs.next().unwrap();
    
    let command_branch = match commands.as_rule() {
        Rule::block => parse_block(commands).unwrap(),
        _ => todo!() //vec![parse_statement(commands).unwrap()],
    };
    
    Stmt::Foreach(name, arr, command_branch)
}

fn parse_if_statement(mut pairs: Pairs<Rule>) -> Result<Stmt, String> {
    // Parse the condition of the 'if' statement
    let condition = parse_expression(pairs.next().unwrap().into_inner())?;

    // Determine the structure of the 'then' branch
    let then_pair = pairs.next().unwrap();
    dbg!(&then_pair.clone().into_inner());
    let then_branch = match then_pair.as_rule() {
        Rule::block => parse_block(then_pair)?,
        _ => vec![parse_statement(then_pair)?],  // Wrap single statement into a vector
    };

    // Attempt to parse the 'else' branch, if it exists
    let else_branch = if let Some(else_pair) = pairs.next() {
        let inner_else = else_pair;
        match inner_else.as_rule() {
            Rule::if_statement => Some(Box::new(parse_if_statement(inner_else.into_inner())?)),
            Rule::block => Some(Box::new(Stmt::Block(parse_block(inner_else)?))),
            _ => Some(Box::new(parse_statement(inner_else)?)),  // Handle single else statement
        }
    } else {
        None
    };

    Ok(Stmt::If(condition, then_branch, else_branch))
}

fn parse_block(pair: Pair<Rule>) -> Result<Vec<Stmt>, String> {
    // Directly parse the inner statements of the block
    pair.into_inner().map(parse_statement).collect()
}

//remember to add statements here if they should be nestable
fn parse_statement(pair: Pair<Rule>) -> Result<Stmt, String> {
    match pair.as_rule() {
        Rule::expression_statement => Ok(parse_print_statement(pair.into_inner())),
        Rule::let_statement => Ok(parse_let_statement(pair.into_inner())),
        Rule::print_statement => Ok(parse_print_statement(pair.into_inner())),
        Rule::connect_command => Ok(parse_connect_statement(pair.into_inner())),
        Rule::if_statement => parse_if_statement(pair.into_inner()),
        Rule::external_definition => Ok(parse_external_statement(pair.into_inner())),
        Rule::block => {
            parse_block(pair).map(Stmt::Block)
        },
        Rule::listen_command => Ok(parse_listen_statement(pair.into_inner())),
        Rule::foreach_statement => Ok(parse_foreach_statement(pair.into_inner())),
        _ => Err(format!("unsupported statement type for nesting: {:?}", &pair.as_rule()))
    }
}


fn parse_external_statement(mut pairs: Pairs<Rule>) -> Stmt {
    let identifier = pairs.next().unwrap().as_str().to_string();

    let mut file = String::new();
    let mut command = String::new();
    let mut args = Vec::new();
    let mut defaults = Vec::new();

    // Now we need to parse the external_body
    let body_pairs = pairs.next().unwrap().into_inner(); // Get the inner pairs of the body

    for pair in body_pairs {
        match pair.as_rule() {
            Rule::file_decl => {
                file = pair.into_inner().next().unwrap().as_str().to_string();
            },
            Rule::args_decl => {
                command = pair.into_inner().next().unwrap().as_str().to_string();
            },
            Rule::defaults_decl => {
                let defaults_pairs = pair.into_inner(); // Get inside the defaults block
                for default_pair in defaults_pairs {
                    let mut inner_pairs = default_pair.into_inner(); // identifier : number
                    let name = inner_pairs.next().unwrap().as_str().to_string();
                    let value = inner_pairs.next().unwrap().as_str().to_string();
                    defaults.push((name, value));
                }
            },
            _ => unreachable!(),
        }
    }

    args = find_substrings_inside_braces(&command).unwrap();
    let defaults_map: HashMap<_, _> = defaults.iter().cloned().collect();

    let defaults_optimized = args.iter()
        .filter_map(|arg| defaults_map.get(arg))
        .cloned()
        .collect::<Vec<String>>();

    Stmt::External(identifier, file, command, args, defaults_optimized)
}

fn find_substrings_inside_braces(text: &str) -> Result<Vec<String>, Box<dyn Error>> {
    let re = Regex::new(r"\{([^{}]*)\}")?;
    let matches = re.captures_iter(text)
        .map(|cap| cap[1].to_string())
        .collect::<Vec<String>>();
    
    Ok(matches)
}

fn parse_expression(mut pairs: Pairs<Rule>) -> Result<Expr, String> {
    
    let mut expr = parse_term(pairs.next().unwrap())?;
    // Process the rest of the pairs
    while let Some(pair) = pairs.peek() {
        match pair.as_rule() {
            Rule::add | Rule::subtract | Rule::cmp => {
                pairs.next(); // consume the operator
                let next_expr =
                    parse_term(pairs.next().ok_or("Expected expression after operator")?)?;
                expr = match pair.as_rule() {
                    Rule::add => Expr::Add(Box::new(expr), Box::new(next_expr)),
                    Rule::subtract => Expr::Sub(Box::new(expr), Box::new(next_expr)),
                    Rule::cmp => Expr::Cmp(Box::new(expr), Box::new(next_expr)),
                    _ => unreachable!(), // We know it's either add or subtract due to the match
                };
            },
            Rule::execute => {

                let execute_pair = pairs.next().unwrap(); // consume the execute
                let func_name = execute_pair.as_str().to_owned();
                let args = execute_pair.into_inner().map(|p| {
                    // Here we convert each Pair into a one-element Pairs
                    parse_expression(p.into_inner())
                }).collect::<Result<Vec<_>, String>>()?;
                expr = Expr::Exec(func_name, args);
            },
            _ => break, // If the next token is not an add or subtract, stop processing
        }
    }

    Ok(expr)
}

fn parse_function_call(pair: Pair<Rule>) -> Result<Expr, String> {
    let mut inner_pairs = pair.into_inner();
    let func_name_pair = inner_pairs.next().unwrap();
    let func_name = func_name_pair.as_str().trim().to_string();

    let mut arguments = Vec::new();

    // Assuming that arguments are separated by commas, which should be parsed out in the grammar
    for arg_pair in inner_pairs {
        let arg_expr = parse_expression(arg_pair.into_inner())?;
        arguments.push(arg_expr);
    }

    Ok(Expr::Exec(func_name, arguments))
}

fn parse_term(pair: Pair<Rule>) -> Result<Expr, String> {
    match pair.as_rule() {
        Rule::number => Ok(Expr::Int(pair.as_str().parse::<i32>().unwrap())),
        Rule::identifier => Ok(Expr::Var(pair.as_str().to_string())),
        Rule::string => {
            let stripped = pair.as_str().trim_matches('"').to_string();
            Ok(Expr::Str(stripped))
        },
        Rule::boolean => Ok(Expr::Bool(pair.as_str().to_string())),
        Rule::symbol => Ok(Expr::Symbol(pair.as_str().to_string())),
        Rule::execute => parse_function_call(pair),
        Rule::array_literal => {
            let elements = pair.into_inner()
                .map(|p| parse_expression(p.into_inner()))
                .collect::<Result<Vec<_>, String>>()?;
            Ok(Expr::Array(elements))
        },
        Rule::array_access => {

            let mut variables= pair.into_inner();
            let name = variables.next().unwrap().as_str().to_string();
            let index = parse_term(variables.next().unwrap().into_inner().next().unwrap()).unwrap();

            Ok(Expr::ArrayAccess(name, Box::new(index)))
        }
        Rule::block_expression => {
            let inner = pair.into_inner();
            let mut statements: Vec<Stmt> = vec![];
            let mut last = Expr::Int(0);
            for stmt in inner {
                if stmt.as_rule() == Rule::expression {
                    last = parse_expression(stmt.into_inner())?;
                } else {
                    statements.push(parse_statement(stmt)?);
                }
            }
            Ok(Expr::BlockExpression(statements, Box::new(last)))
        }
        Rule::range => {
            let mut inner = pair.into_inner();
            let start = Box::new(parse_expression(inner.next().unwrap().into_inner()).unwrap());
            let stop = Box::new(parse_expression(inner.next().unwrap().into_inner()).unwrap());
            Ok(Expr::Range(start, stop))
        }
        _ => Err(format!("unexpected term type {:?}", &pair.as_rule())),
    }
}
