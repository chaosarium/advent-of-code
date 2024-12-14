// recursive solution blows up recursion depth... oops
// also infinite loop detection thinks we're infinitely looping... oops^2
// you'll need to compile the typst with higher MAX_CALL_DEPTH, MAX_ITERATIONS

#let parse(input_text) = {
  let orig = input_text.split("").filter(x => x != "").map(x => int(x))
  let files = orig
    .enumerate()
    .filter(x => calc.even(x.at(0)))
    .map(x => x.at(1))
    .enumerate()
    .map(x => 
      array.range(0, x.at(1)).map(y => x.at(0))
    )
  let spaces = orig
    .enumerate()
    .filter(x => calc.odd(x.at(0)))
    .map(x => x.at(1))
    .enumerate()
    .map(x => 
      array.range(0, x.at(1)).map(y => -1)
    )
  let disk = files
    .enumerate()
    .map(x => 
      if spaces.len() <= x.at(0) {(x.at(1))} 
      else { (x.at(1), spaces.at(x.at(0))) }
    )
    .flatten()

  let lo = 0
  let hi = disk.len() - 1

  let recursiveSolve(disk, lo, hi) = {

    if hi == 0 {
      return (0, ())
    } else if lo > hi and disk.at(hi) == -1 {
      return recursiveSolve(disk, lo, hi - 1)
    } else if lo > hi and disk.at(hi) != -1 {
      let rec = recursiveSolve(disk, lo, hi - 1)
      rec.at(1).push(disk.at(hi))
      return (hi * disk.at(hi) + rec.at(0), rec.at(1))
    } else if lo < hi and disk.at(lo) == -1 and disk.at(hi) != -1 {
      let rec = recursiveSolve(disk, lo + 1, hi - 1)
      rec.at(1).push(disk.at(hi))
      return (disk.at(hi) * lo + rec.at(0), rec.at(1))
    } else if lo < hi and disk.at(lo) == -1 and disk.at(hi) == -1 {
      return recursiveSolve(disk, lo, hi - 1)
    } else if lo < hi and disk.at(lo) != -1 {
      return recursiveSolve(disk, lo + 1, hi)
    } else if lo == hi and disk.at(lo) == -1 {
      return recursiveSolve(disk, lo, hi - 1)
    } else if lo == hi and disk.at(lo) != -1 {
      return recursiveSolve(disk, lo + 1, hi)
    }

    return (0, (9999999,))

  }

  let loopSolve(disk) = {
    let lo = 0
    let hi = disk.len() - 1

    let acc = 0
    while hi >= 0 {
      if lo > hi {
        if disk.at(hi) != -1 {
          acc = acc + hi * disk.at(hi)
        }
        hi = hi - 1
      } else if lo < hi {
        if disk.at(hi) == -1 {
          hi = hi - 1
        } else if disk.at(lo) != -1 {
          lo = lo + 1
        } else {
          acc = acc + disk.at(hi) * lo
          lo = lo + 1 
          hi = hi - 1 
        }
      } else if lo == hi {
        if disk.at(hi) != -1 {
          lo = lo + 1
        } else {
          hi = hi - 1
        }
      }
    }
    return acc

  }

  [
    // text: #text

    // files: #files
    
    // spaces: #spaces

    // disk: #disk.map(x => if x != -1 {str(x)} else {"."}).join("")

    // solution: 
  ]

  // return recursiveSolve(disk, lo, hi).at(0)
  return loopSolve(disk)
  
}

p1 test: *#parse(read("test.txt"))* \
p1 real: *#parse(read("input.txt"))*