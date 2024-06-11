use crate::Expr;
use base64;
use core::panic;
use std::ops::Index;
use std::vec;
use regex::Regex;
use serde_json;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::result::Result;
use std::sync::Arc;

pub fn skip_lines_func() -> Arc<dyn Fn(Vec<Expr>) -> Result<Expr, String> + Send + Sync> {
    Arc::new(|args: Vec<Expr>| match (&args[1]) {
        Expr::Int(i) => {

            let lines = &args[0];

            //we currently only handle when the first argument is a string
            match lines {
                Expr::Str(s) => {
                    let lines_split = s.split('\n');
                    let res = lines_split.skip(1).collect::<Vec<&str>>().join("\n");
                    let value = Expr::Str(res);
                    Ok(value)
                    
                }
                _ => todo!()
            }

        }
        _ => todo!(),
    })

}

pub fn keep_wordcount_func() -> Arc<dyn Fn(Vec<Expr>) -> Result<Expr, String> + Send + Sync> {
    Arc::new(|args: Vec<Expr>| match (&args[1]) {
        Expr::Int(i) => {
            fn count_words(line: &str, word_regex: &Regex) -> usize {
                word_regex.find_iter(line).count()
            }
            if args.len() != 2 {
                return Err("keep_wordcount() needs two arguments".to_string());
            }

            let lines = &args[0];

            let wc = if let Expr::Int(i) = &args[1] {
                *i
            } else {
                return Err ("second arg is not an integer".to_string());
            };

            //regex for finding the words separated by common separators
            let wc_regex = Regex::new(r"[^\s,]+").unwrap();

            //we currently only handle when the first argument is a string
            match lines {
                Expr::Str(s) => {
                    let lines_split = s.split('\n');
                    let mut result = String::new();

                    for line in lines_split {
                        if count_words(line, &wc_regex) == wc as usize {
                            if !result.is_empty() {
                                result.push('\n');
                            }
                            result.push_str(line);
                        }
                    }

                    Ok(Expr::Str(result))
                    
                }
                _ => todo!()
            }

        }
        _ => todo!(),
    })


}

pub fn replace_func() -> Arc<dyn Fn(Vec<Expr>) -> Result<Expr, String> + Send + Sync> {
    Arc::new(|args: Vec<Expr>| match (&args[0], &args[1], &args[2]) {
        (Expr::Str(a), Expr::Str(b), Expr::Str(c)) => {
            //read text from a
            let new_a = a.replace(b, c);
            Ok(Expr::Str(new_a))
        }
        _ => todo!(),
    })
}

pub fn array_from_index() -> Arc<dyn Fn(Vec<Expr>) -> Result<Expr, String> + Send + Sync> {
    Arc::new(|args: Vec<Expr>| match (&args[0], &args[1], &args[2]) {
        
        _ => todo!()
    })
}

pub fn array_from_delimiter_func() -> Arc<dyn Fn(Vec<Expr>) -> Result<Expr, String> + Send + Sync> {
    Arc::new(|args: Vec<Expr>| match (&args[0], &args[1]) {
        (Expr::Str(a), Expr::Str(b)) => {
            let b_transformed = b.replace("\\\\", "\\");
            if b_transformed == "\\n" {
                let matches: Vec<Expr> = a.split("\n").map(|x| Expr::Str(x.to_string())).collect();
                return Ok(Expr::Array(matches));
            }
            let matches: Vec<Expr> = a.split(&b_transformed).map(|x| Expr::Str(x.to_string())).collect();
            Ok(Expr::Array(matches))
        }
        _ => todo!(),
    })
}

pub fn array_from_match_func() -> Arc<dyn Fn(Vec<Expr>) -> Result<Expr, String> + Send + Sync> {
    Arc::new(|args: Vec<Expr>| match (&args[0], &args[1]) {
        (Expr::Str(a), Expr::Str(b)) => {
            let mut reg: Regex;

            match b.as_str() {
                "ipv4" => {
                    let ipv4_part = r"(25[0-5]|2[0-4][0-9]|1[0-9]{2}|[1-9][0-9]|[0-9])";
                    let ipv4_regex = format!(
                        r"{}\.\b{}\b\.\b{}\b\.\b{}",
                        ipv4_part, ipv4_part, ipv4_part, ipv4_part
                    );
                    reg = Regex::new(&ipv4_regex).unwrap();
                }
                "email" => {
                    let email_regex = r"(?i)\b[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}\b";
                    reg = Regex::new(&email_regex).unwrap();
                }
                "number" => {
                    let number_regex = r"-?\d+";
                    reg = Regex::new(&number_regex).unwrap();
                }
                _ => {
                    let b_transformed = b.replace("\\\\", "\\");
                    reg = match Regex::new(&format!(r"{}", b_transformed)) {
                        Ok(re) => re,
                        Err(e) => {
                            println!("invalid regex: {}", e);
                            panic!()
                        }
                    };
                }
            }

            let matches: Vec<Expr> = reg
                .find_iter(a)
                .map(|m| Expr::Str(m.as_str().to_string()))
                .collect();

            Ok(Expr::Array(matches))
        }
        _ => todo!(),
    })
}

pub fn read_func() -> Arc<dyn Fn(Vec<Expr>) -> Result<Expr, String> + Send + Sync> {
    Arc::new(|args: Vec<Expr>| match &args[0] {
        Expr::Str(a) => {
            //read text from a
            let contents = fs::read_to_string(a).expect(&format!("invalid file path: {}", a));
            Ok(Expr::Str(contents))
        }
        _ => todo!(),
    })
}

pub fn write_func() -> Arc<dyn Fn(Vec<Expr>) -> Result<Expr, String> + Send + Sync> {
    Arc::new(|args: Vec<Expr>| match (&args[0]) {
        (Expr::Str(a)) => {
            match (&args[1]) {
                Expr::Str(b) => {
                    fs::write(a, b).expect(&format!("could not write to file: {}", a));
                    Ok(Expr::Bool("True".to_string()))
                }
                Expr::Int(i) => {
                    fs::write(a, i.to_string()).expect(&format!("could not write to file: {}", a));
                    Ok(Expr::Bool("True".to_string()))
                }
                Expr::Array(b) => {
                    let formatted_b = b.iter().map(|x| 
                        match x {
                            Expr::Str(s) => s.clone(),
                            Expr::Int(i) => i.to_string(),
                            _ => todo!(),
                        }
                    ).collect::<Vec<_>>().join("\n");
                    fs::write(a, formatted_b).expect(&format!("could not write to file: {}", a));
                    Ok(Expr::Bool("True".to_string()))
                }
                _ => todo!(),
            }
        }

        _ => todo!(),
    })
}

pub fn base64_func() -> Arc<dyn Fn(Vec<Expr>) -> Result<Expr, String> + Send + Sync> {
    Arc::new(|args: Vec<Expr>| match &args[0] {
        Expr::Str(a) => Ok(Expr::Str(base64::encode(a))),
        _ => todo!(),
    })
}

pub fn base64_decode_func() -> Arc<dyn Fn(Vec<Expr>) -> Result<Expr, String> + Send + Sync> {
    Arc::new(|args: Vec<Expr>| match &args[0] {
        Expr::Str(a) => match base64::decode(a) {
            Ok(bytes) => match String::from_utf8(bytes) {
                Ok(s) => Ok(Expr::Str(s)),
                Err(e) => Err(format!("Failed to convert decoded bytes to string: {}", e)),
            },
            Err(e) => Err(format!("Failed to decode Base64 string: {}", e)),
        },
        _ => Err("base64 decode function requires a string argument".to_string()),
    })
}

pub fn json_encode_func() -> Arc<dyn Fn(Vec<Expr>) -> Result<Expr, String> + Send + Sync> {
    Arc::new(|args: Vec<Expr>| {
        let mut json_object = HashMap::new();
        for chunk in args.chunks(2) {
            if chunk.len() == 2 {
                let pair = (&chunk[0], &chunk[1]);
                match pair {
                    (Expr::Str(l), Expr::Str(r)) => {
                        json_object.insert(l.to_string(), r.to_string());
                    }
                    _ => todo!(),
                };
            }
        }
        Ok(Expr::Str(serde_json::to_string(&json_object).unwrap()))
    })
}

pub fn string_convert_func() -> Arc<dyn Fn(Vec<Expr>) -> Result<Expr, String> + Send + Sync> {
    Arc::new(|args: Vec<Expr>| match &args[0] {
        Expr::Byte(a) => match String::from_utf8(a.clone()) {
            Ok(string) => Ok(Expr::Str(string)),
            Err(e) => Err(format!("Invalid UTF-8 sequence: {}", e)),
        },
        _ => Err("Unsupported argument type; expected Byte".to_string()),
    })
}
