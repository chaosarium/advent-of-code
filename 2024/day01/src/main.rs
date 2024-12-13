#![allow(warnings)]

use std::{collections::HashMap, io};

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
    
    
    // solve 1
    A.sort();
    B.sort();
    
    let mut res: usize = 0;
    for (&x, &y) in A.iter().zip(B.iter()) {
        res += if x > y {x - y} else {y - x};
    }
    
    
    // solve 2
    let mut cnts: HashMap<usize, usize> = HashMap::new();
    for &b in B.iter() {
        *cnts.entry(b).or_insert(0) += 1;
    }
    let mut res2: usize = 0;
    for &a in A.iter() {
        if let Some(&cnt) = cnts.get(&a) {
            res2 += cnt * a;
        }
    }
    
    // print
    println!("part1: {}", res);
    println!("part2: {}", res2);
}
