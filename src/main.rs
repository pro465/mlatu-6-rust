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
            Thing::Quote(q) => write!(
                f,
                "({})",
                q.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .concat()
            ),
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

const NUM_OPERS: u128 = 6;

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, strum::EnumString, strum::EnumIter, strum::Display,
)]
enum Oper {
    #[strum(serialize = "+")]
    Dupe,
    #[strum(serialize = "-")]
    Delete,
    #[strum(serialize = "~")]
    Swap,
    #[strum(serialize = ",")]
    Concat,
    #[strum(serialize = ">")]
    Nest,
    #[strum(serialize = "<")]
    Unnest,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, thiserror::Error)]
#[error("Not enough quotes.")]
struct OperError(bool);

impl Oper {
    pub fn convert(num: u128) -> Oper {
        use Oper::*;
        match num {
            0 => Dupe,
            1 => Swap,
            2 => Concat,
            3 => Nest,
            4 => Unnest,
            5 => Delete,
            _ => unreachable!(),
        }
    }
    pub fn operate(self, term: &mut Vec<Thing>) -> Result<Option<Vec<Thing>>, OperError> {
        if term.len() < self.arity() {
            return Err(OperError(false));
        }
        if term[term.len() - self.arity()..]
            .iter()
            .any(|t| t.as_quote().is_none())
        {
            return Err(OperError(false));
        }
        match self {
            Oper::Dupe => {
                let mut copy = term.last().unwrap().clone();
                term.push(copy);
                Ok(None)
            }
            Oper::Delete => {
                term.pop().unwrap();
                Ok(None)
            }
            Oper::Swap => {
                let len = term.len();
                term.swap(len - 1, len - 2);
                Ok(None)
            }
            Oper::Concat => {
                let temp = term.pop().unwrap().into_quote().unwrap();
                let mut new_quote = term.pop().unwrap().into_quote().unwrap();
                new_quote.extend(temp);
                term.push(Thing::Quote(new_quote));
                Ok(None)
            }
            Oper::Nest => {
                let thing = term.pop().unwrap();
                term.push(Thing::Quote(vec![thing]));
                Ok(None)
            }
            Oper::Unnest => Ok(Some(term.pop().unwrap().into_quote().unwrap())),
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

fn to_str(result: &[Thing], stack: &[Vec<Thing>]) -> String {
    let mut s = result
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .concat();
    for piece in stack.iter().rev() {
        s.extend(piece.iter().rev().map(ToString::to_string));
    }
    s
}

fn decide(log: &[String]) -> bool {
    let s = log.last().unwrap();
    let i = log.len();
    if log[..i - 1]
        .iter()
        .any(|x| s.ends_with(x) || s.starts_with(x))
    {
        return true;
    }
    let last4 = &log[i-4..];
    let last4 = [last4[0].len(),last4[1].len(),last4[2].len(),last4[3].len()];
    if last4[0] < last4[1] && last4[2] < last4[3] {
    let (da, db) = (last4[1] - last4[0], last4[3] - last4[2]);
    if da == db {
        return true;
    }}
    false
}

fn reduce<const C: bool>(
    input: Vec<Thing>,
    mut lim: u64,
    memlim: usize,
) -> Result<Vec<Thing>, OperError> {
    let mut result = Vec::new();
    let mut stack = vec![input];
    let mut log = Vec::new();
    stack[0].reverse();
    let mut flag = false;

    while !stack.is_empty() {
        lim -= 1;
        let s = to_str(&result, &stack);
        if C {
            println!("{}", &s);
        }
        let mut piece = stack.pop().unwrap();
        if lim == 0 || piece.iter().map(|x| x.to_string().len()).sum::<usize>() > memlim {
            flag = true;
            break;
        }

        log.push(s);
        let Some(next) = piece.pop() else {
            continue;
        };

        match next {
            Thing::Char(c) => {
                result.push(Thing::Char(c));
                stack.push(piece);
            }
            Thing::Quote(q) => {
                result.push(Thing::Quote(q));
                stack.push(piece);
            }
            Thing::Oper(o) => match o.operate(&mut result) {
                Ok(Some(new_piece)) => {
                    stack.push(piece);
                    stack.push(new_piece);
                    stack.last_mut().unwrap().reverse();
                }
                Ok(None) => {
                    stack.push(piece);
                }
                Err(e) => return Err(e),
            },
        }
    }

    if C {
        println!("{}", to_str(&result, &stack));
    }

    if flag {
        if decide(&log) {
            return Err(OperError(false));
        }
        if C { println!("{:?}", log.iter().map(String::len).collect::<Vec<_>>()); }
        return Err(OperError(true));
    }
    Ok(result)
}

fn filter(p: &Term) -> bool {
    let twocombs = &[
        (Thing::Oper(Oper::Concat), Thing::Oper(Oper::Delete)),
        (Thing::Oper(Oper::Dupe), Thing::Oper(Oper::Swap)),
        (Thing::Oper(Oper::Nest), Thing::Oper(Oper::Delete)),
        (Thing::Oper(Oper::Nest), Thing::Oper(Oper::Unnest)),
        (Thing::Oper(Oper::Dupe), Thing::Oper(Oper::Delete)),
        (Thing::Oper(Oper::Swap), Thing::Oper(Oper::Swap)),
    ];

    for i in 0..p.len() {
        if i + 1 >= p.len() {
            break;
        }
        if twocombs.iter().any(|(x, y)| *x == p[i] && *y == p[i + 1]) {
            return false;
        }
    }
    for x in p.iter() {
        if let Thing::Quote(v) = x {
            if !filter(v) {
                return false;
            }
        }
    }
    true
}

fn convert(mut num: u128, c: bool) -> (Term, u128) {
    let mut res = Vec::new();
    let modulo = NUM_OPERS + if c { 1 } else { 2 };
    while num > modulo {
        let result = num % modulo + NUM_OPERS + 2 - modulo;
        num /= modulo;

        let node = match result {
            0 => break,
            x if x == NUM_OPERS + 1 => {
                let (t, new_num) = convert(num, false);
                num = new_num;
                Thing::Quote(t)
            }

            _ => {
                let t = Thing::Oper(Oper::convert(result - 1));
                t
            }
        };
        res.push(node);
    }
    (res, num)
}

fn find(args: Term, exp_res: Term, lim: u64, memlim: usize) {
    let mut avg = 0;
    for i in 0.. {
        let (prog, _) = convert(i, true);
        /*avg += prog.iter()
                    .map(|x|x.to_string().len()).sum::<usize>();
        if i%10000==0 {
            println!(
                "avg. length={}, current program: '{}'",
                avg as f64 / 10000.,
                prog.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .concat()
            );
            avg=0;
        }*/

        if !filter(&prog) {
            continue;
        }
        let mut full = args.clone();
        full.extend_from_slice(&prog);
        let Ok(res) = reduce::<false>(full, lim, memlim) else { continue };

        if res == exp_res {
            println!(
                "{}",
                prog.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .concat()
            );
            let mut full = args.clone();
            full.extend_from_slice(&prog);
            reduce::<true>(full, lim, memlim);
            //break;
        }
    }
}

fn nonhalting(n: u64, memlim: usize) {
    for i in 0.. {
        let (prog, _) = convert(i, true);
        if !filter(&prog) {
            continue;
        }
        if let Err(OperError(true)) = reduce::<false>(prog.clone(), n, memlim) {
            println!(
                "{}",
                prog.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .concat()
            );
        }
    }
}

fn halting_random(low: u64, high: u64, memlim: usize) {
    let mut c=0;
    loop {
        let i = rand::random::<u128>();
        let (prog, _) = convert(i, true);
        if !filter(&prog) {
            continue;
        }
        if let Err(OperError(true)) = reduce::<false>(prog.clone(), low, memlim) {
            if let Ok(_) = reduce::<false>(prog.clone(), high, memlim) {
            println!(
                "{c}: {}",
                prog.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .concat()
            );
            reduce::<true>(prog, low, memlim);
            let _ = std::io::stdin().read_line(&mut String::new());
            }
        }
        c+=1;
    }
}

fn main() {
    halting_random(100, 1000, 2000);
    /*find(
        vec![
            Thing::Quote(vec![Thing::Char('B')]),
            Thing::Quote(vec![Thing::Char('A')]),
        ],
        vec![
            Thing::Quote(vec![
            Thing::Quote(vec![Thing::Char('B')]),
            Thing::Char('A'),
            ]),
            //Thing::Char('A'),
            Thing::Quote(vec![Thing::Char('B')]),
            //Thing::Quote(vec![Thing::Char('C')]),
            //Thing::Char('A'),
        ],
         50,
    );*/
}
