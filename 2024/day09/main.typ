// recursive solution blows up recursion depth... oops
// also infinite loop detection thinks we're infinitely looping... oops^2
// you'll need to build the typst compiler from source with higher MAX_CALL_DEPTH, MAX_ITERATIONS

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

  return disk
}

#let solve(disk) = {

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


#let solve2(disk) = {

  let los = (0,0,0,0,0,0,0,0,0,0)
  let hi = disk.len() - 1
  let hi_id = disk.len()

  let acc = 0
  while hi >= 0 {

    let hi_size = disk.at(hi).at(1)
    let hi_elem = disk.at(hi).at(0)
    let hi_start = hi - hi_size + 1


    if hi_elem == none {
      hi -= hi_size
      continue
    }
    
    if hi_elem >= hi_id {
      hi -= 1
      continue
    }
    hi_id = calc.min(hi_id, hi_elem)

    // find lowest fit
    let loc = hi_start
    let loc_size = hi_size
    for size in (1,2,3,4,5,6,7,8,9) {
      if size < hi_size { continue }
      while los.at(size) < hi_start and (disk.at(los.at(size)).at(0) != none or disk.at(los.at(size)).at(1) != size) {
        los.at(size) += 1
      }
      if los.at(size) >= hi_start { continue }
      if los.at(size) < loc {
        loc = los.at(size)
        loc_size = size
      }
    }

    let hi_cost = 0
    let i = 0
    while i < hi_size {
      hi_cost += (loc + i) * hi_elem
      disk.at(loc + i).at(0) = hi_elem
      disk.at(loc + i).at(1) = hi_size
      i += 1
    }
    acc += hi_cost
    if loc != hi_start {
      let loc_residue = loc_size - hi_size
      let residue_start = loc + hi_size
      los.at(loc_size) += loc_size
      los.at(loc_residue) = calc.min(los.at(loc_residue), loc + hi_size)
      
      let i = 0
      while i < loc_residue {
        disk.at(residue_start + i).at(1) = loc_residue
        i += 1
      }

    }

    hi -= hi_size

    // if hi < 0 {
    //   return [
    //     - hi: #hi
    //     - hi_id: #hi_id
    //     - hi_elem: #hi_elem
    //     - hi_size: #hi_size
    //     - hi_start: #hi_start
    //     - loc: #loc
    //     - loc_size: #loc_size
    //     - hi_cost: #hi_cost
    //     - acc: #acc
    //     - los: #los
    //     - disk: #disk
    //   ]
    // }

  }
  return acc
  
}



#let parse2(input_text) = {
  let orig = input_text.split("").filter(x => x != "").map(x => int(x))
  let files = orig
    .enumerate()
    .filter(x => calc.even(x.at(0)))
    .map(x => x.at(1))
    .enumerate()
    .map(x => 
      array.range(0, x.at(1)).map(y => (x.at(0), x.at(1)))
    )
  let spaces = orig
    .enumerate()
    .filter(x => calc.odd(x.at(0)))
    .map(x => x.at(1))
    .enumerate()
    .map(x => 
      array.range(0, x.at(1)).map(y => (none, x.at(1)))
    )
  let disk = files
    .enumerate()
    .map(x => 
      if spaces.len() <= x.at(0) { (x.at(1)) } 
      else { (x.at(1), spaces.at(x.at(0))) }
    )
    .flatten()
    .chunks(2)

  // [
  //   text: #text

  //   files: #files
    
  //   spaces: #spaces

  //   disk: #disk
  //   // #disk.map(x => if x != -1 {str(x)} else {"."}).join("")

  //   solution: #solve2(disk)
  // ]


  return disk
}


p1 test: *#solve(parse(read("test.txt")))* \
p1 real: *#solve(parse(read("input.txt")))* 

p2 test: *#solve2(parse2(read("test.txt")))* \
p2 real: *#solve2(parse2(read("input.txt")))*