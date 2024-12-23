#![allow(warnings)]

use std::usize::{self, MAX};
use std::{fs, io};
use rustc_hash::FxHashMap as HashMap;
use rustc_hash::FxHashSet as HashSet;

fn nbors(i: usize, j: usize, dim: usize, corrupted: &HashSet<(usize, usize)>) -> HashSet<(usize, usize)> {
    let i = i as i64;
    let j = j as i64;
    let dim = dim as i64;
    let res = [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]
        .into_iter().filter(|&(ii, jj)| {
            ii >= 0 && jj >= 0 && ii < dim && jj < dim && (!corrupted.contains(&(ii as usize, jj as usize)))
        })
        .map(|(ii, jj)| (ii as usize, jj as usize))
        .collect();
    res
}

fn read_currupted_set(file_path: &str) -> Vec<(usize, usize)> {
    let input_str = fs::read_to_string(file_path).expect("can't read");
    input_str.split("\n").filter(|&l| l != "")
        .map(|l| {
            let literals: Vec<&str> = l.split(",").collect();
            let i: usize = literals[0].parse().unwrap();
            let j: usize = literals[1].parse().unwrap();
            (i, j)
        })
        .collect()
}

fn solve1(file_path: &str, prefix_len: usize, dim: usize) -> usize {
    let corrupted = HashSet::from_iter(read_currupted_set(file_path)[..prefix_len].iter().cloned());
    bfs(&corrupted, prefix_len, dim)
}

fn bfs(corrupted: &HashSet<(usize, usize)>, prefix_len: usize, dim: usize) -> usize {
    let mut seen: HashSet<(usize, usize)> = HashSet::default();
    let start = (0, 0);
    let target = (dim-1, dim-1);

    let mut depth = 0;
    let mut frontier: HashSet<(usize, usize)> = HashSet::from_iter([start]);
    while !frontier.is_empty() {
        let mut new_frontier = HashSet::default();
        for &(i_u, j_u) in frontier.iter() {
            if (i_u, j_u) == target {
                return depth;
            }
            seen.insert((i_u, j_u));
            let nbors_u = nbors(i_u, j_u, dim, corrupted);
            for &(i_v, j_v) in nbors_u.iter() {
                if !seen.contains(&(i_v, j_v)) { 
                    new_frontier.insert((i_v, j_v));
                } 
            }
        }
        depth += 1;
        frontier = new_frontier;
    }
    
    return usize::MAX; // target unreachable
}

fn solve2(file_path: &str, dim: usize) -> (usize, usize) {
    let corrupted = read_currupted_set(file_path);
    let max_len = corrupted.len();
    
    let search_space: Vec<usize> = (0..max_len).into_iter().collect();
    
    let part_pt = search_space.partition_point(|&prefix_len| {
        let corrupted_prefix = HashSet::from_iter(corrupted[..prefix_len].iter().cloned());
        bfs(&corrupted_prefix, prefix_len, dim) != usize::MAX
    });
        
    corrupted[part_pt-1]
}

fn main() {
    println!("p1 test: {}", solve1("test.txt", 12, 7));
    println!("p1 real: {}", solve1("input.txt", 1024, 71));
    println!("p2 test: {:?}", solve2("test.txt", 7));
    println!("p2 real: {:?}", solve2("input.txt", 71));
}