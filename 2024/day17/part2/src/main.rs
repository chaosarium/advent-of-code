#![allow(warnings)]
use std::{io};

// run algorithm by hand on whiteboard and use this script to check guesses of A
// the Elm interpreter seems to overflow somehow, this doesn't

fn main() {
    
    let input = io::read_to_string(io::stdin()).unwrap();
    let initA: usize = input.trim().parse::<usize>().unwrap();
    
    let mut A: usize = 0;
    let mut B: usize = 0;
    let mut C: usize = 0;
    
    for i in 0..8 {
        A = initA*8 + i;
        print!("{} -> ", A);
        loop {
            B = A % 8;
            B = B ^ 0b010;
            C = A / (0b1 << B);
            B = B ^ C;
            B = B ^ 0b011;
            print!("{},", B % 8);
            A = A / 8;
            if A == 0 { break }
        }
        print!("\n");
    }

}
