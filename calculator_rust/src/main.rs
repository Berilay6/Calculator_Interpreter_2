use std::io::{self, Write};
use std::collections::HashMap; //for keeping variables
use std::iter::Peekable;
use std::str::Chars;

//function for evaluating expressions
fn evaluate(expr: &str, vars: &mut HashMap<String, f64>) -> Result<f64, String> {

    let cleaned_expr: String = expr.chars().filter(|c| !c.is_whitespace()).collect(); // ignore spaces
    if let Some((var, value)) = parse_assignment(&cleaned_expr) {
        vars.insert(var, value);
        return Ok(value);
    }
    let mut chars = cleaned_expr.chars().peekable();
    parse_expr(&mut chars, vars)
}

//for handling assignments
fn parse_assignment(expr: &str) -> Option<(String, f64)> {
    let tokens: Vec<&str> = expr.split('=').map(|s| s.trim()).collect();
    if tokens.len() == 2 {
        if let Ok(value) = tokens[1].parse::<f64>() {
            return Some((tokens[0].to_string(), value));
        }
    }
    None
}


//for addition and substraction
fn parse_expr(chars: &mut Peekable<Chars>, vars: &mut HashMap<String, f64>) -> Result<f64, String> {

    let mut result = parse_term(chars, vars)?;
    while let Some(&ch) = chars.peek() {
        if ch == '+' || ch == '-' {
            chars.next();
            let next_term = parse_term(chars, vars)?;
            if ch == '+' {
                result += next_term;
            } else {
                result -= next_term;
            }
        } else {
            break;
        }
    }
    Ok(result)
}

//for multiplication and division
fn parse_term(chars: &mut Peekable<Chars>, vars: &mut HashMap<String, f64>) -> Result<f64, String> {

    let mut result = parse_factor(chars, vars)?;
    while let Some(&ch) = chars.peek() {
        if ch == '*' || ch == '/' {
            chars.next();
            let next_factor = parse_factor(chars, vars)?;
            if ch == '*' {
                result *= next_factor;
            } else {
                if next_factor == 0.0 {
                    return Err("Division by zero".to_string());
                }
                result /= next_factor;
            }
        } else {
            break;
        }
    }
    Ok(result)
}

//for pharantesed expressions
fn parse_factor(chars: &mut Peekable<Chars>, vars: &mut HashMap<String, f64>) -> Result<f64, String> {

    if let Some(&ch) = chars.peek() {
        if ch == '(' {
            chars.next();
            let result = parse_expr(chars, vars)?;
            if chars.next() != Some(')') {
                return Err("Mismatched parentheses".to_string());
            }
            return Ok(result);
        }
    }
    parse_num_or_var(chars, vars)
}

//seperating numbers and variables
fn parse_num_or_var(chars: &mut Peekable<Chars>, vars: &mut HashMap<String, f64>) -> Result<f64, String> {

    let mut token = String::new();
    while let Some(&ch) = chars.peek() {
        if ch.is_alphanumeric() || ch == '.' {
            token.push(ch);
            chars.next();
        } else {
            break;
        }
    }
    
    if let Ok(num) = token.parse::<f64>() {
        return Ok(num);
    }
    
    vars.get(&token).cloned().ok_or("Undefined variable".to_string())
}

fn main() {
    let mut vars: HashMap<String, f64> = HashMap::new(); //for variables
    loop{
        let mut input = String::new();
        println!("Enter an expression:");
        io::stdout().flush().unwrap();
        io::stdin().read_line(&mut input).expect("Failed to read input");
        let input = input.trim();

        if input == "q" {
            break;
        }
    
        match evaluate(input.trim(), &mut vars) {
            Ok(result) => println!("Result: {}", result),
            Err(e) => println!("Error: {}", e),
            
        }
        println!();
    }
}
