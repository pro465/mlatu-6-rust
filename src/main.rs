use std::fmt;

use strum::{self, IntoEnumIterator};
use thiserror;

type Term = Vec<Thing>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Thing {
    Char(char),
    Quote(Term),
    Oper(Oper),
}

impl fmt::Display for Thing {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Thing::Char(c) => write!(f, "{}", c),
            Thing::Quote(q) => write!(f, "({})", q.iter().map(ToString::to_string).collect::<Vec<_>>().concat()),
            Thing::Oper(o) => write!(f, "{}", o),
        }
    }
}

impl Thing {
    pub fn as_quote(&self) -> Option<&Term> {
        match self {
            Thing::Quote(q) => Some(q),
            _ => None,
        }
    }

    pub fn into_quote(self) -> Option<Term> {
        match self {
            Thing::Quote(q) => Some(q),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[derive(strum::EnumString, strum::EnumIter, strum::Display)]
enum Oper {
    #[strum(serialize = "+")] Dupe,
    #[strum(serialize = "-")] Delete,
    #[strum(serialize = "~")] Swap,
    #[strum(serialize = ",")] Concat,
    #[strum(serialize = ">")] Nest,
    #[strum(serialize = "<")] Unnest,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[derive(thiserror::Error)]
#[error("Not enough quotes.")]
struct OperError;

impl Oper {
    pub fn operate(self, term: &mut Vec<Thing>) -> Result<Option<Vec<Thing>>, OperError> {
        if term.len() < self.arity() {
            return Err(OperError);
        }
        if term[term.len() - self.arity()..].iter().any(|t| t.as_quote().is_none()) {
            return Err(OperError);
        }
        match self {
            Oper::Dupe => {
                let mut copy = term.last().unwrap().clone();
                term.push(copy);
                Ok(None)
            },
            Oper::Delete => {
                term.pop().unwrap();
                Ok(None)
            },
            Oper::Swap => {
                let len = term.len();
                term.swap(len - 1, len - 2);
                Ok(None)
            },
            Oper::Concat => {
                let mut new_quote = term.pop().unwrap().into_quote().unwrap().clone();
                new_quote.extend(term.pop().unwrap().into_quote().unwrap());
                term.push(Thing::Quote(new_quote));
                Ok(None)
            },
            Oper::Nest => {
                let thing = term.pop().unwrap();
                term.push(Thing::Quote(vec![thing]));
                Ok(None)
            },
            Oper::Unnest => {
                Ok(Some(term.pop().unwrap().into_quote().unwrap()))
            },
        }
    }

    pub fn arity(self) -> usize {
        match self {
            Oper::Dupe => 1,
            Oper::Delete => 1,
            Oper::Swap => 2,
            Oper::Concat => 2,
            Oper::Nest => 1,
            Oper::Unnest => 1,
        }
    }
}

fn show(result: &[Thing], stack: &[Vec<Thing>]) {
    print!("{}", result.iter().map(ToString::to_string).collect::<Vec<_>>().concat());
    for piece in stack.iter().rev() {
        print!("{}", piece.iter().rev().map(ToString::to_string).collect::<Vec<_>>().concat());
    }
    println!();
}


fn reduce(input: Vec<Thing>) -> Result<Vec<Thing>, OperError> {
    let mut result = Vec::new();
    let mut stack = vec![input];
    stack[0].reverse();

    while !stack.is_empty() {
        show(&result, &stack);

        let mut piece = stack.pop().unwrap();
        let Some(next) = piece.pop() else {
            continue;
        };

        match next {
            Thing::Char(c) => {
                result.push(Thing::Char(c));
                stack.push(piece);
            },
            Thing::Quote(q) => {
                result.push(Thing::Quote(q));
                stack.push(piece);
            },
            Thing::Oper(o) => {
                match o.operate(&mut result) {
                    Ok(Some(new_piece)) => {
                        stack.push(piece);
                        stack.push(new_piece);
                        stack.last_mut().unwrap().reverse();
                    },
                    Ok(None) => {
                        stack.push(piece);
                    },
                    Err(e) => return Err(e),
                }
            }
        }
    }

    show(&result, &stack);
    Ok(result)
}


fn main() {
    let x = reduce(vec![
        Thing::Quote(vec![Thing::Char('B')]),
        Thing::Quote(vec![Thing::Char('A')]),
        Thing::Oper(">".parse().unwrap()),
        Thing::Oper("~".parse().unwrap()),
        Thing::Oper(">".parse().unwrap()),
        Thing::Oper(",".parse().unwrap()),
        Thing::Oper("+".parse().unwrap()),
        Thing::Oper("<".parse().unwrap()),
        Thing::Oper(">".parse().unwrap()),
        Thing::Oper("~".parse().unwrap()),
        Thing::Oper(",".parse().unwrap()),
        Thing::Oper("~".parse().unwrap()),
        Thing::Oper("<".parse().unwrap()),
        Thing::Oper(">".parse().unwrap()),
        Thing::Oper(",".parse().unwrap()),

    ]);
    println!("{:?}", x);
}
