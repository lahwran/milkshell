
extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;
use pest::iterators::Pair;

#[derive(Parser)]
#[grammar = "milkshell.pest"]
pub struct MilkshellParser;

fn walk(pair: Pair<Rule>, depth: usize) {
    println!("{}{:?}: {}", " ".repeat(depth*2), pair.as_rule(), pair.as_str());
    for _x in pair.into_inner().map(|x| { walk(x, depth+1) } ) {}
}

fn parse_milkshell(file: &str) {
    println!("start");
    let ms = MilkshellParser::parse(Rule::milkshell, file).expect("Parsing Failed").next().unwrap();
    println!("done parsing");
    walk(ms, 0);
}

fn main() {
    let s = "derp herp     herk 'derp derp' herp | 'derp derp derp' \n | derk derk derk | derp | derpderp|derp| derp  derp| derp derp\n\n";
    parse_milkshell(s);

}
