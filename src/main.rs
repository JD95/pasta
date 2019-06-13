use std::convert::AsRef;
use std::str;
extern crate combine;
use combine::error::ParseError;
use combine::parser::char::{char, digit, letter, spaces};
use combine::stream::Stream;
use combine::{attempt, between, choice, many1, parser, Parser};

/// Represents abstract syntax tree expressions within the language
enum AST<S> {
    Lambda(S, Box<AST<S>>),
    App(Box<AST<S>>, Box<AST<S>>),
    Sym(S),
    Int(i32),
    Add(Box<AST<S>>, Box<AST<S>>),
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

fn add<S>(left: AST<S>, right: AST<S>) -> AST<S> {
    AST::Add(Box::new(left), Box::new(right))
}

impl<S: AsRef<str> + ToString> ToString for AST<S> {
    fn to_string(&self) -> String {
        match self {
            AST::Lambda(x, exp) => {
                "(\\".to_string() + &x.to_string() + " -> " + &exp.to_string() + &")".to_string()
            }
            AST::App(func, arg) => func.to_string() + " " + &arg.to_string(),
            AST::Sym(s) => s.to_string(),
            AST::Int(i) => i.to_string(),
            AST::Add(x, y) => "add ".to_string() + &x.to_string() + " " + &y.to_string(),
        }
    }
}

impl<S: Clone> Clone for AST<S> {
    fn clone(&self) -> Self {
        match self {
            AST::Lambda(x, exp) => AST::Lambda(x.clone(), exp.clone()),
            AST::App(func, arg) => AST::App(func.clone(), arg.clone()),
            AST::Sym(s) => AST::Sym(s.clone()),
            AST::Int(i) => AST::Int(i.clone()),
            AST::Add(x, y) => AST::Add(x.clone(), y.clone()),
        }
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
fn subst<S: Clone + PartialEq>(target: &S, val: AST<S>, exp: AST<S>) -> AST<S> {
    match exp {
        AST::Lambda(x, inner) => {
            if x == *target {
                AST::Lambda(x, inner)
            } else {
                lam(x, subst(target, val, *inner))
            }
        }
        AST::App(func, arg) => app(subst(target, val.clone(), *func), subst(target, val, *arg)),
        AST::Add(x, y) => add(subst(target, val.clone(), *x), subst(target, val, *y)),
        AST::Sym(s) => {
            if s == *target {
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
        AST::Add(x, y) => add(change_symbol_rep(f, *x), change_symbol_rep(f, *y)),
        AST::Sym(x) => sym(f(x)),
        AST::Int(i) => int(i),
    }
}

/// Evaluates the given AST expression, potentially failing
fn eval(exp: AST<String>) -> Result<AST<String>, String> {
    println!("{}", &exp.to_string());
    match exp {
        AST::App(func, arg) => match eval(*func) {
            Result::Ok(AST::Lambda(sym, body)) => eval(subst(&sym, *arg, *body)),
            _ => Result::Err("Invalid application!".to_string()),
        },
        AST::Add(x, y) => match eval(*x) {
            Result::Ok(AST::Int(left)) => match eval(*y) {
                Result::Ok(AST::Int(right)) => Result::Ok(AST::Int(left + right)),
                _ => Result::Err("Bad additions".to_string()),
            },
            _ => Result::Err("Bad additions".to_string()),
        },
        AST::Sym(s) => match s.as_ref() {
            "add" => Result::Ok(lam(
                "x".to_string(),
                lam(
                    "y".to_string(),
                    add(sym("x".to_string()), sym("y".to_string())),
                ),
            )),
            _ => Result::Err("undefined symbol".to_string()),
        },
        x => Result::Ok(x),
    }
}

/// expr_ implements the parser for the language.
/// Grammar:
/// <expr>   := <appl> | <e>
/// <appl>   := <e> <expr>
/// <e>      := <parens> | <lambda> | <math> | <symbol> | <int>
/// <parens> := '(' <expr> ')'
/// <lambda> := '\' <symbol> '->' <expr>
/// <int>    := <digit> <int> | <digit>
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
    let lex_arr = || (char('-'), char('>')).skip(skip_spaces());
    let lambda = || {
        (
            lex_char('\\'),
            word().skip(skip_spaces()),
            lex_arr(),
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

    // appl := <e> <expr>
    let appl = (e(), expr()).map(|t| app(t.0, t.1)).skip(skip_spaces());

    // expr := <appl> | <e>
    choice((attempt(appl), e())).skip(skip_spaces())
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

fn main() {
    let input = "(\\x -> add x 10) 5";
    match expr().parse(input) {
        Ok(exp) => match eval(exp.0) {
            Result::Ok(r) => println!("{}", r.to_string()),
            Result::Err(e) => println!("Eval Error: {}", e),
        },
        Err(e) => println!("Parse Error: {}", e),
    }
}
