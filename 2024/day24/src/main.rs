#![allow(warnings)]

use std::{fs, io};
use std::collections::{BTreeMap, BTreeSet, HashMap as HashMap};
use std::collections::HashSet as HashSet;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Gate {
    AND, 
    XOR,
    OR
}

type Wire = String;
type Instr = (Wire, Gate, Wire, Wire);

fn parse(input_path: &str) -> (BTreeMap<Wire, bool>, Vec<Instr>) {
    let input_str = fs::read_to_string(input_path).expect("can't read");
    let two_halfs: Vec<&str> = input_str.trim().split("\n\n").collect();
    let init_lines: Vec<&str> = two_halfs[0].split("\n").collect();
    let gate_lines: Vec<&str> = two_halfs[1].split("\n").collect();
    
    let init_values: BTreeMap<Wire, bool> = init_lines.iter().map(|l| {
        let chunks: Vec<&str> = l.split(':').collect();
        let wire = chunks[0].trim().to_string();
        let value = chunks[1].trim() != "0";
        (wire, value)
    }).collect();
       
    let gates: Vec<Instr> = gate_lines.iter().map(|l| {
        let chunks: Vec<&str> = l.split(" ").collect();
        let wire1 = chunks[0].to_string();
        let gate = match chunks[1] {
            "AND" => Gate::AND,
            "XOR" => Gate::XOR,
            "OR" => Gate::OR,
            _ => panic!("?"),
        };
        let wire2 = chunks[2].to_string();
        let out_wire = chunks[4].to_string();
        (wire1, gate, wire2, out_wire)
    }).collect();
    
    (init_values, gates)
}

fn eval(in1: bool, in2: bool, gate: Gate) -> bool {
    match gate {
        Gate::AND => in1 & in2,
        Gate::XOR => in1 ^ in2,
        Gate::OR  => in1 | in2,
    }
}

fn part1(input_path: &str) -> usize {
    let (init_values, instrs) = parse(input_path);
    
    let mut assignments = init_values.clone();
    let mut pending: HashMap<Wire, BTreeSet<usize>> = HashMap::default();
    
    fn try_reprocess(
        wire: Wire, 
        pending: &mut HashMap<Wire, BTreeSet<usize>>, 
        assignments: &mut BTreeMap<Wire, bool>,
        instrs: &Vec<Instr>,
    ) {
        match pending.get(&wire) {
            None => (),
            Some(pending_instr) => {
                let mut succeeded: BTreeSet<usize> = BTreeSet::default();
                for &idx in pending_instr {
                    let (wire1, gate, wire2, out_wire) = instrs[idx].clone();
                    if assignments.contains_key(&out_wire) { 
                        succeeded.insert(idx);
                        continue
                    }
                    match (assignments.get(&wire1), assignments.get(&wire2)) {
                        (Some(&x), Some(&y)) => {
                            let out_value = eval(x, y, gate);
                            assignments.insert(out_wire.clone(), out_value);
                            succeeded.insert(idx);
                        },
                        _ => ()
                    }
                }
                pending.entry(wire).and_modify(|x| *x = x.difference(&succeeded).cloned().collect());
                for succeeded_idx in succeeded {
                    let (.., out_wire) = instrs[succeeded_idx].clone();
                    try_reprocess(out_wire, pending, assignments, instrs);
                }
            }
        }
    }
    
    for idx in 0..instrs.len() {
        let (wire1, gate, wire2, out_wire) = instrs[idx].clone();
        if assignments.contains_key(&out_wire) { continue }
        match (assignments.get(&wire1), assignments.get(&wire2)) {
            (Some(&x), Some(&y)) => {
                let out_value = eval(x, y, gate);
                assignments.insert(out_wire.clone(), out_value);
                try_reprocess(out_wire, &mut pending, &mut assignments, &instrs);
            },
            (Some(&x), None) => {
                pending.entry(wire2).or_default().insert(idx);
            },
            (None, Some(&y)) => {
                pending.entry(wire1).or_default().insert(idx);
            },
            (None, None) => {
                pending.entry(wire1).or_default().insert(idx);
                pending.entry(wire2).or_default().insert(idx);
            }
        }
    }
    
    // for (k, v) in assignments.iter() {
    //     println!("\t{} -> {}", k, if *v {"1"} else {"0"});
    // }
    
    let mut answer: usize = 0;
    let mut sig: usize = 1;
    for (k, v) in assignments.iter() {
        if k.chars().next().unwrap() == 'z' {
            answer += sig * (if *v {1} else {0});
            sig *= 2;
        }
    }
    
    return answer;
}


fn main() {
    println!("p1 test: {}", part1("test.txt"));
    println!("p1 test2: {}", part1("test2.txt"));
    println!("p1 real: {}", part1("input.txt"));
}