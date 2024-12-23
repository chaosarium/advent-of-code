import gleam/dict
import gleam/int
import gleam/result
import gleam/list
import gleam/string
import gleam/io
import simplifile

pub fn main() {
  let #(p1, p2) = part1("test.txt")
  io.println("p1 / p2 test: " <> int.to_string(p1) <> " / " <> int.to_string(p2))
  let #(p1, p2) = part1("input.txt")
  io.println("p1 / p2 real: " <> int.to_string(p1) <> " / " <> int.to_string(p2))
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
    |> list.filter(fn(l) {l != ""})
 
  let p1answer = patterns
    |> list.map(check_one_pattern(towels, _))
    |> list.map(fn(b) {case b {True -> 1 False -> 0}})
    |> int.sum
  
  // slow version
  // let p2answer = patterns
  //   |> list.map(sum_one_pattern(towels, _))
  //   |> int.sum
  
  let #(p2answer, _) = list.fold(
    patterns, 
    #(0, dict.new()), 
    fn(state, pattern) { 
      let #(sum_acc, memo) = state
      let #(pattern_answer, new_memo) = sum_one_pattern_dp(towels, pattern, memo) 
      #(pattern_answer + sum_acc, new_memo)
    }
  )
  
  #(p1answer, p2answer)
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

// part 2: num ways to make one pattern
fn sum_one_pattern(towels: List(String), pattern: String) -> Int {
  case string.length(pattern) {
    0 -> 1
    _ -> int.sum(list.map(towels, fn(towel) {
      case string.starts_with(pattern, towel) {
        False -> 0
        True -> sum_one_pattern(towels, string.drop_start(pattern, string.length(towel)))
      }
    }))
  }
}

fn sum_one_pattern_dp(towels: List(String), pattern: String, memo: dict.Dict(String, Int)) -> #(Int, dict.Dict(String, Int)) {
  case string.length(pattern) {
    0 -> #(1, memo)
    _ -> case dict.get(memo, pattern) {
      Ok(memoised) -> #(memoised, memo)
      _ -> {
        let #(instance_answer, new_memo) = list.fold(
          towels,
          #(0, memo),
          fn(state, towel) {
            let #(sum_acc, memo) = state
            case string.starts_with(pattern, towel) {
              False -> #(sum_acc, memo)
              True -> {
                let #(subprob_answer, new_memo) = sum_one_pattern_dp(towels, string.drop_start(pattern, string.length(towel)), memo)
                #(sum_acc + subprob_answer, new_memo)
              }
            }
          }
        )
        let new_new_memo = dict.insert(new_memo, pattern, instance_answer)
        #(instance_answer, new_new_memo)
      }
    }
  }
}