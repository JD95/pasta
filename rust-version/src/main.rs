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
use combine::{attempt, between, choice, many1, parser, Parser};
use std::iter;

/// Represents abstract syntax tree expressions within the language
#[derive(Debug)]
enum AST<S> {
    App(Box<AST<S>>, Box<AST<S>>),
    PrimOp(S, Vec<AST<S>>),
    Lambda(S, Box<AST<S>>),
    Sym(S),
    Int(i32),
    Double(f64),
    Bool(bool),
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

fn dble<S>(n: f64) -> AST<S> {
    AST::Double(n)
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

                format!("(prim_{} [{}{}])", s.to_string(), head, rest)
            }
            AST::Sym(s) => s.to_string(),
            AST::Int(i) => i.to_string(),
            AST::Double(f) => f.to_string(),
            AST::Bool(b) => b.to_string(),
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
            AST::Double(i) => AST::Double(i.clone()),
            AST::Bool(b) => AST::Bool(b.clone()),
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

    let float = || (int(), char('.'), int());

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
            attempt(
                float().map(|t| AST::Double((format!("{}.{}", t.0, t.2)).parse::<f64>().unwrap())),
            ),
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
        AST::Double(i) => dble(i),
        AST::Bool(b) => AST::Bool(b),
    }
}

struct SymTable(pub HashMap<String, AST<String>>);

struct Runtime {
    sym_table: SymTable,
}

fn init_runtime() -> Runtime {
    Runtime {
        sym_table: predefined_symbols(),
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

    insert_func("intAdd", vec!["x", "y"]);
    insert_func("intSub", vec!["x", "y"]);
    insert_func("intMul", vec!["x", "y"]);
    insert_func("intDiv", vec!["x", "y"]);
    insert_func("intLt", vec!["x", "y"]);
    insert_func("intGt", vec!["x", "y"]);
    insert_func("intEq", vec!["x", "y"]);
    insert_func("intLte", vec!["x", "y"]);
    insert_func("intGte", vec!["x", "y"]);

    insert_func("dblAdd", vec!["x", "y"]);
    insert_func("dblSub", vec!["x", "y"]);
    insert_func("dblMul", vec!["x", "y"]);
    insert_func("dblDiv", vec!["x", "y"]);
    insert_func("dblLt", vec!["x", "y"]);
    insert_func("dblGt", vec!["x", "y"]);
    insert_func("dblEq", vec!["x", "y"]);
    insert_func("dblLte", vec!["x", "y"]);
    insert_func("dblGte", vec!["x", "y"]);

    insert_func("if", vec!["x", "y", "z"]);
    insert_func("and", vec!["x", "y"]);
    insert_func("or", vec!["x", "y"]);
    insert_func("not", vec!["x"]);

    table.insert("true".to_string(), AST::Bool(true));
    table.insert("false".to_string(), AST::Bool(false));

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

fn check_dbl(x: AST<String>) -> Result<f64, String> {
    match x {
        AST::Double(d) => Ok(d),
        _ => Err(format!(
            "Type mismatch, expecting a 'double' value but got '{}'",
            x.to_string()
        )),
    }
}

fn check_bool(x: AST<String>) -> Result<bool, String> {
    match x {
        AST::Bool(b) => Ok(b),
        _ => Err(format!(
            "Type mismatch, expecting a `bool` value but got '{}'",
            x.to_string()
        )),
    }
}

fn prim_op<T, P, F>(
    inputs: Vec<AST<String>>,
    rt: &Runtime,
    check: &P,
    f: F,
) -> Result<AST<String>, String>
where
    F: Fn(T) -> AST<String>,
    P: Fn(AST<String>) -> Result<T, String>,
{
    let mut inputs = inputs.into_iter();
    let arg1 = inputs.next().ok_or("Not enough args")?;

    let x = eval(arg1, rt).and_then(check)?;

    print_result(Ok(f(x)))
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

fn prim_if(inputs: Vec<AST<String>>, rt: &Runtime) -> Result<AST<String>, String> {
    let mut inputs = inputs.into_iter();
    let arg1 = inputs.next().ok_or("Not enough args")?;
    let arg2 = inputs.next().ok_or("Not enough args")?;
    let arg3 = inputs.next().ok_or("Not enough args")?;

    let b = eval(arg1, rt).and_then(check_bool)?;
    Ok(if b { arg2 } else { arg3 })
}

fn eval_primop(s: String, inputs: Vec<AST<String>>, rt: &Runtime) -> Result<AST<String>, String> {
    match s.as_ref() {
        // Base Int operations
        "intAdd" => prim_bin_op(inputs, rt, &check_int, |x, y| AST::Int(x + y)),
        "intSub" => prim_bin_op(inputs, rt, &check_int, |x, y| AST::Int(x - y)),
        "intMul" => prim_bin_op(inputs, rt, &check_int, |x, y| AST::Int(x * y)),
        "intDiv" => prim_bin_op(inputs, rt, &check_int, |x, y| AST::Int(x / y)),
        "intLt" => prim_bin_op(inputs, rt, &check_int, |x, y| AST::Bool(x < y)),
        "intGt" => prim_bin_op(inputs, rt, &check_int, |x, y| AST::Bool(x > y)),
        "intEq" => prim_bin_op(inputs, rt, &check_int, |x, y| AST::Bool(x == y)),
        "intLte" => prim_bin_op(inputs, rt, &check_int, |x, y| AST::Bool(x <= y)),
        "intGte" => prim_bin_op(inputs, rt, &check_int, |x, y| AST::Bool(x >= y)),

        // Base Double operations
        "dblAdd" => prim_bin_op(inputs, rt, &check_dbl, |x, y| AST::Double(x + y)),
        "dblSub" => prim_bin_op(inputs, rt, &check_dbl, |x, y| AST::Double(x - y)),
        "dblMul" => prim_bin_op(inputs, rt, &check_dbl, |x, y| AST::Double(x * y)),
        "dblDiv" => prim_bin_op(inputs, rt, &check_dbl, |x, y| AST::Double(x / y)),
        "dblLt" => prim_bin_op(inputs, rt, &check_dbl, |x, y| AST::Bool(x < y)),
        "dblGt" => prim_bin_op(inputs, rt, &check_dbl, |x, y| AST::Bool(x > y)),
        "dblEq" => prim_bin_op(inputs, rt, &check_dbl, |x, y| AST::Bool(x == y)),
        "dblLte" => prim_bin_op(inputs, rt, &check_dbl, |x, y| AST::Bool(x <= y)),
        "dblGte" => prim_bin_op(inputs, rt, &check_dbl, |x, y| AST::Bool(x >= y)),

        // Branching and Logic operations
        "if" => prim_if(inputs, rt),
        "and" => prim_bin_op(inputs, rt, &check_bool, |x, y| AST::Bool(x && y)),
        "or" => prim_bin_op(inputs, rt, &check_bool, |x, y| AST::Bool(x || y)),
        "not" => prim_op(inputs, rt, &check_bool, |x| AST::Bool(!x)),

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
        AST::Sym(s) => match rt.sym_table.0.get(&s) {
            Some(e) => print_result(Ok(e.clone())),
            None => Err("undefined symbol".to_string()),
        },
        x => Ok(x),
    }
}

fn accum(s: &str) -> String {
    let groups = s
        // convert the string into an iterator of indices and chars
        .char_indices()
        // map over each char, transforming them into the chunks
        // like Aaaa, Bbb, D, etc.
        .map(|(i, c)| {
            // Create new iterator with the uppercase as the first
            // element
            iter::once(c.to_uppercase())
                // to_uppercase changes the type of iterator,
                // flatten will make it a char iterator again
                .flatten()
                // chain on an iterator for the lower case letters
                .chain(
                    // repeat creates an infinite stream of the same
                    // value
                    iter::repeat(c.to_lowercase())
                        // take i which is the position and thus the
                        // number we need from the infinite iterator
                        .take(i)
                        // to_lowercase changes the iterator type so
                        // flatten will return it to a char iterator
                        .flatten(),
                )
        });

    let tail = groups
        // clone the iterator since we'll be changing it
        .clone()
        // skip the first element so there won't be dashes
        // in a single char input
        .skip(1)
        // map each chunk into a new iterator with '-' prepended
        // and then chain them all together
        .flat_map(|group| iter::once('-').chain(group));

    // chain the new tail with an unaltered first chunk
    groups.take(1).flatten().chain(tail).collect()
}

fn main() {
    let input = "dblAdd 0.5 0.5";
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