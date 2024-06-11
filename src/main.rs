use core::panic;
use std::collections::HashMap;
use std::error::Error;
use std::ptr::null;
use std::{env, fs, io, process, thread};
use std::process::{Command, ExitStatus};

mod builtin;

mod parser;
use parser::{build_ast, Expr, MyParser, Program, Rule, Stmt};
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use regex::Regex;
use serde_json::value::Index;

use std::io::{BufRead, BufReader, BufWriter, Read, Write};
use std::net::{TcpStream, TcpListener};
use std::time::Duration;

#[macro_use]
extern crate pest_derive;
extern crate base64;

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return Ok(()); // Exit early if no filename is provided
    }
    let filename = &args[1];
    let input = fs::read_to_string(filename)?;

    let re = Regex::new(r"(\b\w+)\(\)").unwrap();

    // Replace all matches with the word followed by ("")
    let modified_input = re.replace_all(&input, r#"$1("")"#).to_string();
    
    process_input(modified_input).unwrap();

    Ok(())
}

pub fn process_input(input: String) -> Result<(), Box<dyn Error>> {
    let parse_result = MyParser::parse(Rule::program, &input).expect("Failed to parse"); // Consider proper error handling here
    let program = build_ast(parse_result);

    let mut env = Environment::setup();
    if let Err(e) = env.run_program(&program) {
        eprintln!("Runtime error: {}", e);
    }

    Ok(())
}

#[derive(Default, Clone)]
struct Environment {
    variables: HashMap<String, Expr>,
}


impl Environment {
    fn setup() -> Self {

        use builtin::*;

        let mut variables: HashMap<String, Expr> = HashMap::new();

        // Symbols
        let nullbyte: Vec<u8> = vec![b'\x00'];

        // Store the symbols
        variables.insert("NULL".to_string(), Expr::Byte(nullbyte));

        // Store the functions in the variables map
        variables.insert("base64".to_string(), Expr::Func(base64_func()));
        variables.insert("base64_decode".to_string(), Expr::Func(base64_decode_func()));
        variables.insert("json".to_string(), Expr::Func(json_encode_func()));
        variables.insert("string".to_string(), Expr::Func(string_convert_func()));
        variables.insert("read".to_string(), Expr::Func(read_func()));
        variables.insert("array_from_match".to_string(), Expr::Func(array_from_match_func()));
        variables.insert("write".to_string(), Expr::Func(write_func()));
        variables.insert("replace".to_string(), Expr::Func(replace_func()));
        variables.insert("array_from_delimiter".to_string(), Expr::Func(array_from_delimiter_func()));
        variables.insert("keep_wordcount".to_string(), Expr::Func(keep_wordcount_func()));
        variables.insert("keep_lines_after".to_string(), Expr::Func(skip_lines_func()));

        Environment { variables }
    }

    fn eval_expr(&self, expr: &Expr) -> Result<Expr, String> {
        match expr {
            Expr::Int(value) => Ok(Expr::Int((*value))),
            Expr::Var(name) => self
                .variables
                .get(name)
                .cloned()
                .ok_or_else(|| format!("Undefined variable `{}`", name))
                .and_then(|x| match x {
                    Expr::Int(i) => Ok(Expr::Int(i)),
                    Expr::Str(s) => Ok(Expr::Str(s)),
                    Expr::Byte(s) => Ok(Expr::Byte(s)),
                    Expr::Array(s) => Ok(Expr::Array(s)),
                    Expr::Bool(b) => Ok(Expr::Bool(b)),
                    Expr::Var(s) => Ok(Expr::Var(s)),
                    Expr::Func(f) => Ok(Expr::Func(f)),
                    Expr::External(a, b, c) => Ok(Expr::External(a, b, c)),
                    Expr::UserFunction(a, b) => Ok(Expr::UserFunction(a, b)),
                    _ => {
                        todo!()
                    },
                }),
            Expr::Add(left, right) => {
                let left_val = self.eval_expr(left)?;
                let right_val = self.eval_expr(right)?;
                match (left_val, right_val) {
                    (Expr::Int(l), Expr::Int(r)) => Ok(Expr::Int(l + r)),
                    (Expr::Str(l), Expr::Str(r)) => Ok(Expr::Str(l + &r)),
                    (Expr::Str(l), Expr::Int(r)) => Ok(Expr::Str(l + &r.to_string())),
                    (Expr::Str(l), Expr::Byte(r)) => {
                        let mut bytes = l.as_bytes().to_vec();
                        for byte in r.iter() {
                            bytes.push(*byte);
                        }
                        Ok(Expr::Byte(bytes))
                    }
                    (Expr::Byte(l), Expr::Str(r)) => {
                        let mut bytes = r.as_bytes().to_vec();
                        for byte in l.iter() {
                            bytes.push(*byte);
                        }
                        Ok(Expr::Byte(bytes))
                    }
                    _ => Err("Can't add these types together".to_string()),
                }
            }
            Expr::Sub(left, right) => {
                let left_val = self.eval_expr(left)?;
                let right_val = self.eval_expr(right)?;
                match (left_val, right_val) {
                    (Expr::Int(l), Expr::Int(r)) => Ok(Expr::Int(l - r)),
                    _ => Err("Cannot subtract non-integer values".to_string()),
                }
            }
            Expr::Cmp(left, right) => {
                let left_val = self.eval_expr(left)?;
                let right_val = self.eval_expr(right)?;
                match (left_val, right_val) {
                    (Expr::Int(l), Expr::Int(r)) => {if l == r {Ok(Expr::Bool("True".to_string()))} else {Ok(Expr::Bool("False".to_string()))} } ,
                    (Expr::Str(l), Expr::Str(r)) => {if l == r {Ok(Expr::Bool("True".to_string()))} else {Ok(Expr::Bool("False".to_string()))} },
                    (Expr::Byte(l), Expr::Byte(r)) => {if l == r {Ok(Expr::Bool("True".to_string()))} else {Ok(Expr::Bool("False".to_string()))} }
                    (Expr::Bool(l), Expr::Bool(r)) => {if l == r {Ok(Expr::Bool("True".to_string()))} else {Ok(Expr::Bool("False".to_string()))} }
                    _ => Err("Cmp operations only implemented for values of type Integer, String, Bool or Byte".to_string() )
                }
            }
            Expr::Str(s) => Ok(Expr::Str(s.clone())),
            Expr::Bool(b) => Ok(Expr::Bool(b.clone())),
            Expr::Exec(name, args) => {
                let func = self
                    .variables
                    .get(name)
                    .ok_or_else(|| format!("No function or external named '{}' found.", name))?;

                if let Expr::External(c, c_args, defaults) = func {

                    let evaluated_args = args
                        .iter()
                        .map(|arg| self.eval_expr(arg))
                        .collect::<Result<Vec<Expr>, String>>()?;

                    //replace args with user input
                    let mut command = c.clone();
                    for i in (0..args.len()) {
                        let replace = format!("{{{}}}",c_args[i]);
                        if let Expr::Str(s) = &evaluated_args[i] {
                            command = command.replace(&replace, &s);
                        }
                    }
                    //replace remaining with defaults
                    if args.len() < c_args.len() && defaults.len() != 0 {
                        for i in (args.len() .. c_args.len()) {
                            let replace = format!("{{{}}}",c_args[i]);
                            command = command.replace(&replace, &defaults[i-1]);
                        }
                    }

                    command = command.replace("\"", "");
                    //execute command (only unix right now)
                    let output = if cfg!(target_os = "windows") {
                        Command::new("cmd")
                            .args(["/C", &command])
                            .output()
                            .expect("failed to execute process")
                    } else {
                        Command::new("sh")
                            .arg("-c")
                            .arg(command)
                            .output()
                            .expect("failed to execute process")
                    };
                    let mut bytes = Vec::new();
                    if output.status.success() {
                        bytes = output.stdout;
                    } else {
                        bytes = output.stderr;
                    }
                    //return output as a Expr::Str
                    Ok(Expr::Byte(bytes))
                } else if let Expr::Func(f) = func {
                    // Evaluate all arguments
                    let evaluated_args = args
                        .iter()
                        .map(|arg| self.eval_expr(arg))
                        .collect::<Result<Vec<Expr>, String>>()?;

                    // Execute the function with the evaluated arguments
                    f(evaluated_args)
                } else if let Expr::UserFunction(params, block) = func {
                    let mut func_environment = self.clone();
                    //set the variables
                    let evaluated_args = args
                        .iter()
                        .map(|arg| self.eval_expr(arg))
                        .collect::<Result<Vec<Expr>, String>>()?;

                    if evaluated_args.len() != params.len() {
                        panic!("not enough arguments supplied for function {}, needs {}", name, params.len());
                    }
                    //since they have the same length we can safely set the function environment
                    for i in (0..params.len()) {
                        func_environment.variables.insert(params[i].clone(), evaluated_args[i].clone());
                    }
                    let res = func_environment.eval_expr(&block)?;
                    Ok(res)
                } else {
                    Err(format!("'{}' is not a function.", name))
                }
            }
            Expr::Symbol(name) => self
                .variables
                .get(name)
                .cloned()
                .ok_or_else(|| format!("Invalid symbol {}", name))
                .and_then(|x| match x {
                    Expr::Byte(s) => Ok(Expr::Byte(s)),
                    Expr::Int(s) => Ok(Expr::Int(s)),
                    Expr::Str(s) => Ok(Expr::Str(s)),
                    _ => todo!(),
                }),
            Expr::Array(elements) => {
                let evaluated_elements = elements.iter().map(|el| self.eval_expr(el)).collect::<Result<Vec<_>, _>>()?;
                Ok(Expr::Array(evaluated_elements))
            },
            Expr::ArrayAccess(name, index) => {
                let ev_index = match self.eval_expr(index).unwrap() {
                    Expr::Int(i) => i as usize,
                    _ => panic!(),
                };
                self.variables.get(name).cloned().ok_or_else(|| format!("use of undeclared array: {}", &name)).and_then(
                    |x| match x {
                        Expr::Array(s) => Ok(s[ev_index].clone()),
                        _ => todo!()
                    }
                )
            }
            Expr::BlockExpression(s, f) => {
                let mut temp = self.clone();
                for stmt in s {
                    temp.execute_stmt(stmt)?;
                }
                let result = temp.eval_expr(f).unwrap();
                Ok(result)
            }
            Expr::Range(start,stop ) => {
                let mut array: Vec<Expr> = vec![];
                let ev_start = self.eval_expr(start).unwrap();
                let ev_stop = self.eval_expr(stop).unwrap();
                match (ev_start, ev_stop) {
                    (Expr::Int(a), Expr::Int(b)) => {
                        for i in (a..b) {
                            array.push(Expr::Int(i));
                        }
                    }
                    _ => todo!(),
                }
                Ok(Expr::Array(array))
            }
            _ => {
                todo!()
            } // Add other operations like subtraction here
        }
    }

    fn print_statement(&mut self, expr: &Expr) -> Result<(), String> {
        let value = self.eval_expr(expr)?;
                match value {
                    Expr::Int(x) => print!("{}", x),
                    Expr::Str(x) => {
                        print!("{}", x);
                    }
                    Expr::Byte(x) => print!("{:?}", x),
                    Expr::Array(x) => {
                        print!("[");
                        for (i, e) in x.iter().enumerate() {
                            self.print_statement(e)?;
                            if i < x.len() - 1 {
                                print!(",");
                            }
                        }
                        print!("]");
                    },
                    Expr::Bool(b) => print!("{:?}", b),
                    _ => todo!(),
                }
                Ok(())
    }

    fn foreach_statement(&mut self, name: String, arr: &Expr, statements: &Vec<Stmt>) -> Result<(), String> {
        let evaluated_arr = self.eval_expr(arr).unwrap();
        let mut elements: Vec<Expr>;

        match evaluated_arr {
            Expr::Array(a) => elements = a,
            Expr::Str(s) => if let Expr::Array(a) = self.variables.get(&s).cloned().unwrap() {elements = a} else { panic!("requested variable was not an array")}
            _ => panic!("requested variable was not an array") 
        }
        
        for e in elements {
            self.variables.insert(name.clone(), e);
            for s in statements {
                self.execute_stmt(s).unwrap();
            }
        }

        Ok(())
    }

    fn execute_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Assign(name, expr) => {
                let value = self.eval_expr(expr)?;
                self.variables.insert(name.clone(), value);
                Ok(())
            }
            Stmt::Print(expr) => {
                let e = self.print_statement(expr);
                println!("");
                e
            }
            Stmt::Connect(ip, port, message) => {
                let x = self.eval_expr(message).unwrap();

                match x {
                    Expr::Int(i) => execute_connect_tcp(ip, *port, &i.to_string()),
                    Expr::Str(s) => execute_connect_tcp(ip, *port, &s),
                    _ => todo!(),
                }
            }
            Stmt::Listen(port) => {
                execute_listener_statement(port.to_owned());
                Ok(())
            }
            Stmt::External(identifier, name, command, args, defaults) => {
                let full_command = format!("{} {}",name, command);
                let external_expression = Expr::External(full_command, args.clone(), defaults.clone());
                self.variables.insert(identifier.clone(), external_expression);
                Ok(())
            }
            Stmt::Foreach(name, arr, statements) => {
                self.foreach_statement(name.clone(), arr, statements)
            }
            Stmt::If(condition, if_statements, else_statements) => {
                let value = self.eval_expr(condition)?;
                match value {
                    // Match against `Expr::Bool` variant correctly
                    Expr::Bool(b) if b == "True" => {
                        // Directly iterate and execute statements if the condition is true
                        for s in if_statements {
                            self.execute_stmt(s)?;
                        }
                    },
                    Expr::Bool(b) if b == "False" => {
                        if let Some(else_stmts) = else_statements {
                            // Execute else statements if the condition is false
                            for s in else_statements {
                                self.execute_stmt(s)?;
                            }
                        }
                    },
                    _ => return Err("Condition expression did not evaluate to a boolean.".into()),
                }
                Ok(())
            }
            Stmt::FunctionDeclaration(identifier, userfunction) => {
                self.variables.insert(identifier.clone(), userfunction.clone());
                Ok(())
            }
            Stmt::Block(statements) => {
                // Execute each statement in the block
                for stmt in statements {
                    self.execute_stmt(stmt)?;
                }
                Ok(())
            }
        }
    }

    fn run_program(&mut self, program: &Program) -> Result<(), String> {
        for stmt in &program.0 {
            self.execute_stmt(stmt)?;
        }
        Ok(())
    }
}



fn execute_listener_statement(port: String) {
    let address = format!("0.0.0.0:{}", port);
    let listener = TcpListener::bind(address).expect("could not bind on port specified");
    println!("listener active on port {}", port);

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                thread::spawn(|| handle_incoming(stream));
            }
            Err(e) => {
                println!("failed to establish connection: {}", e);
            }
        }
    }

}

fn handle_incoming(mut stream: TcpStream) {
    println!("incomming connection from: {}", stream.peer_addr().unwrap());

    let mut w = stream.try_clone().expect("Failed to clone stream for writing");
    let mut r = BufReader::new(stream);

    let (mut stdin, mut stdout) = (io::stdin(), io::stdout());

    let reader_t = thread::spawn(move || {
        let mut buffer = [0; 1024];
        while let Ok(size) = r.read(&mut buffer) {
            if size == 0 {
                break;
            }
            stdout.write_all(&buffer[..size]).expect("failed to write output from client");
            stdout.flush().expect("failed to clean output");
        }
    });

    let mut input = String::new();
    while stdin.read_line(&mut input).expect("Failed to read from stdin") > 0 {
        if input.trim_end() == "exit" {
            break;
        }
        w.write_all(input.as_bytes()).expect("Failed to write to stream");
        w.flush().expect("Failed to flush writer");
        input.clear();
    }

    // Wait for the reader thread to finish
    let _ = reader_t.join();

    
}

fn execute_connect_tcp(ip: &str, port: u16, message: &str) -> Result<(), String> {
    let address = format!("{}:{}", ip, port);
    let mut stream =
        TcpStream::connect(address).map_err(|e| format!("Failed to connect: {}", e))?;

    // Disable Nagle's Algorithm
    stream
        .set_nodelay(true)
        .map_err(|e| format!("Failed to set nodelay: {}", e))?;

    // Set a read timeout
    stream
        .set_read_timeout(Some(Duration::from_secs(10)))
        .map_err(|e| format!("Failed to set read timeout: {}", e))?;

    stream
        .write_all(message.as_bytes())
        .and_then(|_| stream.flush())
        .map_err(|e| format!("Failed to send message: {}", e))?;

    let mut buffer = [0; 1024];
    let mut response = String::new();

    // Read the response from the server
    let size = stream
        .read(&mut buffer)
        .map_err(|e| format!("Failed to receive response: {}", e))?;
    response = String::from_utf8(buffer[..size].to_vec())
        .map_err(|e| format!("Failed to read response: {}", e))?;

    println!("Received response: {}", response);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_should_never_fail() {
        assert!(1 == 1);
    }

    #[test]
    fn test_let_statement() {
        let input = "x = 5;".to_string();
        assert!(process_input(input).is_ok());
    }

    #[test]
    fn test_print_statement() {
        let x = "print \"hello world\";".to_string();
        assert!(process_input(x).is_ok());
    }

    #[test]
    fn test_addition() {
        let x = "x = 1 + 1;".to_string();
        assert!(process_input(x).is_ok());
    }

    #[test]
    fn test_subtraction() {
        let x = "x = 1 - 1;".to_string();
        assert!(process_input(x).is_ok());
    }

    #[test]
    fn test_base64_encode() {
        let x = "print base64(\"test\");".to_string();
        assert!(process_input(x).is_ok());
    }
    #[test]
    fn test_append_strings() {
        let x = "x = \"test\"; y = x + \"string\";".to_string();
        assert!(process_input(x).is_ok());
    }

    #[test]
    fn test_number_string_append() {
        let x = "x = \"hello\"; y = x + 2;".to_string();
        assert!(process_input(x).is_ok());
    }

    #[test]
    fn test_nullbyte() {
        let x = "x = \"hello\" + NULL;".to_string();
        assert!(process_input(x).is_ok());
    }

    #[test]
    fn test_json() {
        let x = "x = json(\"test\", \"value\", \"test2\", \"value2\");".to_string();
        assert!(process_input(x).is_ok());
    }

    #[test]
    fn test_array_declare() {
        let x = "x = [1,2,3];".to_string();
        assert!(process_input(x).is_ok());
    }

    #[test]
    fn test_function_dec() {
        let x = "
        x(a) = {a}; 
        x(1);
        ".to_string();
        assert!(process_input(x).is_ok());
    }

    #[test]
    fn test_foreach() {
        let x = "foreach x in (0..5) {print x;};".to_string();
        assert!(process_input(x).is_ok());
    }

    #[test]
    fn test_external() {
        let x = "external test {file: \"echo\" args: \"{{text}}\"};
        test(\"hello\");
        ".to_string();
        assert!(process_input(x).is_ok());
    }
    #[test]
    fn test_if_else() {
        let x = "
        if (1==1) {
            print \"true\";
        } else {
            print \"false\";
        };
        ".to_string();
        assert!(process_input(x).is_ok());
    }

}
