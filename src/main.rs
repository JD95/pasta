use std::str;
#[macro_use]
extern crate combine;
use combine::error::{ParseError, StdParseResult};
use combine::parser::char::{char, letter, spaces};
use combine::stream::state::State;
use combine::stream::{Positioned, Stream};
use combine::{attempt, between, choice, many1, parser, sep_by, Parser};

enum AST<S> {
    Lambda(S, Box<AST<S>>),
    App(Box<AST<S>>, Box<AST<S>>),
    Sym(S),
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

impl<S: AsRef<str> + ToString> ToString for AST<S> {
    fn to_string(&self) -> String {
        match self {
            AST::Lambda(x, exp) => {
                "(\\".to_string() + &x.to_string() + " -> " + &exp.to_string() + &")".to_string()
            }
            AST::App(func, arg) => func.to_string() + " " + &arg.to_string(),
            AST::Sym(s) => s.to_string(),
        }
    }
}

impl<S: Clone> Clone for AST<S> {
    fn clone(&self) -> Self {
        match self {
            AST::Lambda(x, exp) => AST::Lambda(x.clone(), exp.clone()),
            AST::App(func, arg) => AST::App(func.clone(), arg.clone()),
            AST::Sym(s) => AST::Sym(s.clone()),
        }
    }
}

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
        AST::Sym(s) => {
            if s == *target {
                val
            } else {
                sym(s)
            }
        }
    }
}

fn change_symbol_rep<S, T, F>(f: &F, exp: AST<S>) -> AST<T>
where
    F: Fn(S) -> T,
{
    match exp {
        AST::Lambda(x, inner) => lam(f(x), change_symbol_rep(f, *inner)),
        AST::App(func, arg) => app(change_symbol_rep(f, *func), change_symbol_rep(f, *arg)),
        AST::Sym(x) => sym(f(x)),
    }
}

fn eval<S: PartialEq + Clone>(exp: AST<S>) -> Result<AST<S>, String> {
    match exp {
        AST::App(func, arg) => match *func {
            AST::Lambda(sym, body) => Result::Ok(subst(&sym, *arg, *body)),
            _ => Result::Err("Invalid application!".to_string()),
        },
        x => Result::Ok(x),
    }
}

// `impl Parser` can be used to create reusable parsers with zero overhead
fn expr_<I>() -> impl Parser<Input = I, Output = AST<String>>
where
    I: Stream<Item = char>,
    // Necessary due to rust-lang/rust#24159
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    let word = || many1(letter());

    // A parser which skips past whitespace.
    // Since we aren't interested in knowing that our expression parser
    // could have accepted additional whitespace between the tokens we also silence the error.
    let skip_spaces = || spaces().silent();

    //Creates a parser which parses a char and skips any trailing whitespace
    let lex_char = |c| char(c).skip(skip_spaces());
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
    let parens = || between(lex_char('('), lex_char(')'), expr());
    let e = || choice((parens(), lambda(), word().map(AST::Sym))).skip(skip_spaces());
    let appl = (e(), expr()).map(|t| app(t.0, t.1)).skip(skip_spaces());
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
    let input = "(\\x -> x) y";
    match expr().parse(input) {
        Ok(exp) => match eval(exp.0) {
            Result::Ok(r) => println!("{}", r.to_string()),
            Result::Err(e) => println!("Eval Error: {}", e),
        },
        Err(e) => println!("Parse Error: {}", e),
    }
}
