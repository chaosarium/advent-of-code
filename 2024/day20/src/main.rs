#![allow(warnings)]

use std::usize::{self, MAX};
use std::{fs, io};
use std::collections::{BTreeMap, HashMap as HashMap};
use std::collections::HashSet as HashSet;

fn nbors(coord: (usize, usize), map: &Vec<Vec<MapObject>>) -> Vec<(usize, usize)> {
    let (i, j) = coord;
    let res = [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]
        .into_iter().filter(|&(ii, jj)| {
            map[ii][jj] != MapObject::Wall
        })
        .map(|(ii, jj)| (ii as usize, jj as usize))
        .collect();
    res
}

fn locs_at_cheat_end(coord: (usize, usize), map: &Vec<Vec<MapObject>>) -> Vec<((usize, usize), usize)> {
    let i = coord.0 as i64;
    let j = coord.1 as i64;
    let height = map.len() as i64;
    let width = map[0].len() as i64;
    let res = [
               ((i-2,j), 2),
             ((i-1,j-1), 2),   ((i-1,j+1), 2),
            ((i, j-2), 2),      ((i, j+2), 2),
             ((i+1,j-1), 2),   ((i+1,j+1), 2),
               ((i+2,j), 2),
    ].into_iter().filter(|&((ii, jj), jump_cost)| {
            ii >= 0 && jj >= 0 && ii < height && jj < width
        })
        .map(|((ii, jj), jump_cost)| ((ii as usize, jj as usize), jump_cost))
        .collect();
    res

}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum MapObject {
    Wall,
    Empty,
    Start,
    End,
}

fn read_map(file_path: &str) -> Vec<Vec<MapObject>> {
    let input_str = fs::read_to_string(file_path).expect("can't read");
    input_str.split("\n").filter(|&l| l != "")
        .map(|l| {
            l.chars().map(|c| {
                match c {
                    '#' => MapObject::Wall,
                    '.' => MapObject::Empty,
                    'S' => MapObject::Start,
                    'E' => MapObject::End,
                    _ => panic!()
                }
            }).collect()
        })
        .collect()
}

fn find_first(map: &Vec<Vec<MapObject>>, query: MapObject) -> (usize, usize) {
    for (i, row) in map.iter().enumerate() {
        for (j, cell) in row.iter().enumerate() {
            if &query == cell {
                return (i, j)
            }
        }
    };
    panic!();
}

fn bfs(map: &Vec<Vec<MapObject>>, source: (usize, usize)) -> HashMap<(usize, usize), usize> {
    let mut dists: HashMap<(usize, usize), usize> = HashMap::default();

    let mut depth = 0;
    let mut frontier: HashSet<(usize, usize)> = HashSet::from_iter([source]);
    while !frontier.is_empty() {
        let mut new_frontier = HashSet::default();
        for &u in frontier.iter() {
            if !dists.contains_key(&u) {
                dists.insert(u, depth);
            }
            let nbors_u = nbors(u, &map);
            for &v in nbors_u.iter() {
                if !dists.contains_key(&v) { 
                    new_frontier.insert(v);
                } 
            }
        }
        depth += 1;
        frontier = new_frontier;
    }
    
    dists
}

fn manhattan_dist((i1, j1): (usize, usize), (i2, j2): (usize, usize)) -> usize {
    (usize::max(i1, i2) - usize::min(i1, i2)) + (usize::max(j1, j2) - usize::min(j1, j2))
}

fn solve1(file_path: &str) -> usize {
    let map = read_map(file_path);
    let end = find_first(&map, MapObject::End);
    let start = find_first(&map, MapObject::Start);
    
    let dists_from_start = bfs(&map, start);
    let dists_from_end = bfs(&map, end);
    let no_cheating_best = dists_from_start.get(&end).unwrap().clone();
    
    let mut stats: BTreeMap<usize, usize> = BTreeMap::default();
    for (i, row) in map.iter().enumerate() {
        for (j, cell) in row.iter().enumerate() {
            let u = (i, j);
            if cell != &MapObject::Wall {
                for (v@(iv, jv), _) in locs_at_cheat_end(u, &map) {
                    
                    // activate cheat at u and end at v
                    //     v
                    //   v 1 v
                    // v 1 u 1 v
                    //   v 1 v
                    //     v
                    
                    match dists_from_end.get(&v) {
                        None => {},
                        Some (u_to_end) => {
                            let new_dist = dists_from_start.get(&u).unwrap() + dists_from_end.get(&v).unwrap() + manhattan_dist(u, v);
                            if new_dist < no_cheating_best {
                                let time_saved = no_cheating_best - new_dist;
                                *stats.entry(time_saved).or_insert(0) += 1;
                            }
                        }
                    }
                }
            }
        }
    };
    
    // println!("{:?}", stats);
    
    stats.iter().filter(|&(k, v)| k >= &100).map(|(k, v)| *v).sum()
}


fn main() {
    println!("p1 test: {}", solve1("test.txt"));
    println!("p1 real: {}", solve1("input.txt"));
}