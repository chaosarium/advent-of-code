#![allow(warnings)]

use std::io;

fn main() {
    
    // parse
    let input = io::read_to_string(io::stdin()).unwrap();
    let mut A: Vec<usize> = input.split("\n").filter(|&s| s != "")
        .map(|s| s.split(" ").next().unwrap().parse::<usize>().unwrap())
        .collect()
    ;
    let mut B: Vec<usize> = input.split("\n").filter(|&s| s != "")
        .map(|s| s.split(" ").last().unwrap().parse::<usize>().unwrap())
        .collect()
    ;
    
    
    // solve
    A.sort();
    B.sort();
    
    let mut res: usize = 0;
    for (&x, &y) in A.iter().zip(B.iter()) {
        res += if x > y {x - y} else {y - x};
    }
    
    
    // print
    println!("res: {}", res);
}
