use std::collections::HashMap;
use std::convert::AsRef;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::iter::Iterator;
use std::str;
extern crate combine;
use combine::error::ParseError;
use combine::parser::char::{char, digit, letter, spaces, string};
use combine::stream::Stream;
use combine::{between, choice, many1, parser, Parser};

/// Represents abstract syntax tree expressions within the language
#[derive(Debug)]
enum AST<S> {
    App(Box<AST<S>>, Box<AST<S>>),
    PrimOp(S, Vec<AST<S>>),
    Lambda(S, Box<AST<S>>),
    Sym(S),
    Int(i32),
}

fn sym<S>(s: S) -> AST<S> {
    AST::Sym(s)
}

fn lam<S>(input: S, body: AST<S>) -> AST<S> {
    AST::Lambda(input, Box::new(body))
}

fn app<S>(func: AST<S>, arg: AST<S>) -> AST<S> {
    AST::App(Box::new(func), Box::new(arg))
}

fn int<S>(n: i32) -> AST<S> {
    AST::Int(n)
}

impl<S: AsRef<str> + ToString + Debug> ToString for AST<S> {
    fn to_string(&self) -> String {
        match self {
            AST::Lambda(x, exp) => format!("(\\{} -> {})", &x.to_string(), &exp.to_string()),
            AST::App(func, arg) => format!("({} {})", &func.to_string(), &arg.to_string()),
            AST::PrimOp(s, inputs) => {
                let mut xs = inputs.iter().map(|x| x.to_string());
                let head = xs.next().unwrap_or("".to_string());
                let rest = xs
                    .map(|x| ", ".to_string() + &x)
                    .fold("".to_string(), |x, y| x + &y);

                format!("({} [{}{}])", s.to_string(), head, rest)
            }
            AST::Sym(s) => s.to_string(),
            AST::Int(i) => i.to_string(),
        }
    }
}

impl<S: Clone> Clone for AST<S> {
    fn clone(&self) -> Self {
        match self {
            AST::Lambda(x, exp) => AST::Lambda(x.clone(), exp.clone()),
            AST::App(func, arg) => AST::App(func.clone(), arg.clone()),
            AST::PrimOp(s, env) => AST::PrimOp(s.clone(), env.clone()),
            AST::Sym(s) => AST::Sym(s.clone()),
            AST::Int(i) => AST::Int(i.clone()),
        }
    }
}

/// expr_ implements the parser for the language.
/// Grammar:
/// <app>    := <e> | <app> <e>
/// <e>      := <parens> | <lambda> | <symbol> | <int>
/// <parens> := '(' <expr> ')'
/// <lambda> := '\' <symbol> '->' <expr>
/// <int>    := <digit> <int> | <digit>
/// <symbol> := <letter> <symbol> | <letter>
fn expr_<I>() -> impl Parser<Input = I, Output = AST<String>>
where
    I: Stream<Item = char>,
    // Necessary due to rust-lang/rust#24159
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let int = || many1(digit());

    let word = || many1(letter());

    // A parser which skips past whitespace.
    // Since we aren't interested in knowing that our expression parser
    // could have accepted additional whitespace between the tokens we also silence the error.
    let skip_spaces = || spaces().silent();

    //Creates a parser which parses a char and skips any trailing whitespace
    let lex_char = |c| char(c).skip(skip_spaces());

    // lambda := '\' <symbol> '->' <expr>
    let lex_str = |s| string(s).skip(skip_spaces());

    let lambda = || {
        (
            lex_char('\\'),
            word().skip(skip_spaces()),
            lex_str("->"),
            expr(),
        )
            .map(|t| lam(t.1, t.3))
    };

    // parens := '(' <expr> ')'
    let parens = || between(lex_char('('), lex_char(')'), expr());

    // e := <parens> | <lambda> | <symbol> | <int>
    let e = || {
        choice((
            parens(),
            lambda(),
            word().map(AST::Sym),
            int().map(|s: String| AST::Int(s.parse::<i32>().unwrap())),
        ))
        .skip(skip_spaces())
    };

    // app := <e> | <app> <e>
    many1(e()).map(|v: Vec<AST<String>>| {
        let mut es = v.into_iter();
        let head = es.next().unwrap();
        es.fold(head, |a, b| app(a, b))
    })
}

// As this expression parser needs to be able to call itself recursively `impl Parser` can't
// be used on its own as that would cause an infinitely large type. We can avoid this by using
// the `parser!` macro which erases the inner type and the size of that type entirely which
// lets it be used recursively.
//
// (This macro does not use `impl Trait` which means it can be used in rust < 1.26 as well to
// emulate `impl Parser`)
parser! {
    fn expr[I]()(I) -> AST<String>
    where [I: Stream<Item = char>]
    {
        expr_()
    }
}

/// Performs substitution over `exp`. Any symbols matching `target`
/// is swapped out with `val`. This is used to implement lambda
/// application.
///
/// subst x t (f (x y) (z x y)) = f (t y) (z t y)
///
/// It does not propogate into lambda terms with an input matching
/// the target symbol. This allows names to be shadowed under lambdas.
///
/// subst x t ((\x -> f x) x) = (\x -> f x) t
fn subst(target: &String, val: AST<String>, exp: AST<String>) -> AST<String> {
    match exp {
        AST::Lambda(x, inner) => {
            if x == *target {
                AST::Lambda(x, inner)
            } else {
                lam(x, subst(target, val, *inner))
            }
        }
        AST::App(func, arg) => app(subst(target, val.clone(), *func), subst(target, val, *arg)),
        AST::PrimOp(s, inputs) => AST::PrimOp(
            s,
            inputs
                .into_iter()
                .map(|i| subst(target, val.clone(), i))
                .collect(),
        ),
        AST::Sym(s) => {
            if s == *target {
                println!("subst | {} ~> {}", &target.to_string(), &val.to_string());
                val
            } else {
                sym(s)
            }
        }
        _ => exp,
    }
}

/// Allows for the representation of symbols to be changed.
/// This is useful when hand coding examples or test cases with
/// `&'staitc str`, but then evaluating with `String`.
fn change_symbol_rep<S, T, F>(f: &F, exp: AST<S>) -> AST<T>
where
    F: Fn(S) -> T,
{
    match exp {
        AST::Lambda(x, inner) => lam(f(x), change_symbol_rep(f, *inner)),
        AST::App(func, arg) => app(change_symbol_rep(f, *func), change_symbol_rep(f, *arg)),
        AST::PrimOp(s, env) => AST::PrimOp(
            f(s),
            env.into_iter().map(|e| change_symbol_rep(f, e)).collect(),
        ),
        AST::Sym(x) => sym(f(x)),
        AST::Int(i) => int(i),
    }
}

struct SymTable(pub HashMap<String, AST<String>>);

struct Runtime {
    symTable: SymTable,
}

fn init_runtime() -> Runtime {
    Runtime {
        symTable: predefined_symbols(),
    }
}

fn prim_op_lambda(op_name: String, inputs: Vec<String>) -> AST<String> {
    let op = AST::PrimOp(
        op_name.clone(),
        inputs.clone().into_iter().map(AST::Sym).collect(),
    );
    inputs.into_iter().rev().fold(op, |r, x| lam(x, r))
}

fn predefined_symbols() -> SymTable {
    let mut table = HashMap::new();

    // add = \x -> \y -> x + y
    let mut insert_func = |op: &'static str, inputs: Vec<&'static str>| {
        table.insert(
            op.to_string(),
            prim_op_lambda(
                op.to_string(),
                inputs.into_iter().map(|x| x.to_string()).collect(),
            ),
        )
    };

    insert_func("add", vec!["x", "y"]);

    SymTable(table)
}
fn print_result(e: Result<AST<String>, String>) -> Result<AST<String>, String> {
    e.map(|r| {
        println!("eval  | {}", &r.to_string());
        r
    })
}

fn check_int(x: AST<String>) -> Result<i32, String> {
    match x {
        AST::Int(i) => Ok(i),
        _ => Err(format!(
            "Type mismatch, expecting an `int` value but got '{}'",
            x.to_string()
        )),
    }
}

fn prim_bin_op<T, P, F>(
    inputs: Vec<AST<String>>,
    rt: &Runtime,
    check: &P,
    f: F,
) -> Result<AST<String>, String>
where
    F: Fn(T, T) -> AST<String>,
    P: Fn(AST<String>) -> Result<T, String>,
{
    let mut inputs = inputs.into_iter();
    let arg1 = inputs.next().ok_or("Not enough args")?;
    let arg2 = inputs.next().ok_or("Not enough args")?;

    let x = eval(arg1, rt).and_then(check)?;
    let y = eval(arg2, rt).and_then(check)?;

    print_result(Ok(f(x, y)))
}

fn eval_primop(s: String, inputs: Vec<AST<String>>, rt: &Runtime) -> Result<AST<String>, String> {
    match s.as_ref() {
        "add" => prim_bin_op(inputs, rt, &check_int, |x, y| AST::Int(x + y)),
        _ => Err(format!("Undefined primop {}", s)),
    }
}

/// Evaluates the given AST expression, potentially failing
fn eval(exp: AST<String>, rt: &Runtime) -> Result<AST<String>, String> {
    println!("focus | {}", &exp.to_string());
    match exp {
        AST::App(func, arg) => match eval(*func, rt) {
            Ok(AST::Lambda(sym, body)) => eval(subst(&sym, *arg, *body), rt),
            _ => Err("Invalid application!".to_string()),
        },
        AST::PrimOp(s, inputs) => eval_primop(s, inputs, rt),
        AST::Sym(s) => match rt.symTable.0.get(&s) {
            Some(e) => print_result(Ok(e.clone())),
            None => Err("undefined symbol".to_string()),
        },
        x => Ok(x),
    }
}

fn main() {
    let input = "(\\x -> add x 10) (add 10 2)";
    match expr().parse(input) {
        Ok(exp) => {
            println!("{}", &exp.0.to_string());
            match eval(exp.0, &init_runtime()) {
                Ok(r) => println!("{}", r.to_string()),
                Err(e) => println!("Eval Error: {}", e),
            }
        }
        Err(e) => println!("Parse Error: {}", e),
    }
}
