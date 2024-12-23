import gleam/bool
import gleam/set
import gleam/int
import gleam/result
import gleam/option
import gleam/list
import gleam/string
import gleam/io
import simplifile

pub fn main() {
  io.println("p1 test: " <> int.to_string(part1("test.txt")))
  io.println("p1 real: " <> int.to_string(part1("input.txt")))
}

fn part1(input_path: String) {
  let assert Ok(input_str) = simplifile.read(from: input_path)
  
  let towels = string.split(input_str, on: "\n") 
    |> list.first 
    |> result.unwrap("")
    |> string.split(",")
    |> list.map(string.trim)
    
  let patterns = string.split(input_str, on: "\n")
    |> list.drop(2)
    |> list.filter(fn(l) { l != ""})
 
  let p1per_line_answers = patterns
    |> list.map(check_one_pattern(towels, _))
        
  let p1answer = p1per_line_answers
    |> list.map(fn(b) {case b {True -> 1 False -> 0}})
    |> int.sum
  
  p1answer
}

fn check_one_pattern(towels: List(String), pattern: String) -> Bool {
  case string.length(pattern) {
    0 -> True
    _ -> list.any(towels, fn(towel) {
      case string.starts_with(pattern, towel) {
        False -> False
        True -> check_one_pattern(towels, string.drop_start(pattern, string.length(towel)))
      }
    })
  }
}