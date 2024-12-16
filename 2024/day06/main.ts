import * as fs from 'node:fs';

function parse(input_text: string): string[][] {
  const lines = input_text.split('\n').filter(line => line.trim() !== '')
  const grid = lines.map(line => line.split(''))
  return grid
}

function locateGuard(grid: string[][]): [number, number] {
  for (let i = 0; i < grid.length; i++) {
    for (let j = 0; j < grid.length; j++) {
      if (grid[i][j] == '^') {
        return [i, j]
      }
    }
  }
  throw Error
}

interface answer {
  looping: boolean,
  uniques: number,
  part2answer: number,
}
function trace(grid: string[][], coordInit: [number, number], doPart2: boolean): answer {
  var i = coordInit[0]
  var j = coordInit[1]
  var dir: "N" | "S" | "W" | "E" = 'N'
  const height = grid.length
  const width = grid[0].length
  var part2answer = 0

  function intoKey(i: number, j: number): number {
    return (i * width + j)
  }  
  function outofKey(k: number): [number, number] {
    return [(k - k % width) / width, k % width]
  }
  function intoDirectedKey(i: number, j: number, dir: "N" | "S" | "W" | "E"): number {
    let base = intoKey(i, j) * 4
    if (dir === 'N') { return base + 0 }
    else if (dir === 'S') { return base + 1 }
    else if (dir === 'W') { return base + 2 }
    else if (dir === 'E') { return base + 3 }
    else { throw Error }
  }  
  function inGrid(i: number, j: number): boolean {
    return 0 <= i && i < height && 0 <= j && j < width
  }
  function isBlocking(i: number, j: number): boolean {
    return grid[i][j] === '#'
  }

  var visited: Set<number> = new Set()
  var directedVisited = new Set()
  var isLooping = false
  
  while (true) {
    visited.add(i * width + j)
    directedVisited.add(intoDirectedKey(i, j, dir))
    var iNew = i
    var jNew = j
    if (dir === 'N') { iNew = i - 1 }
    else if (dir === 'S') { iNew = i + 1 }
    else if (dir === 'W') { jNew = j - 1 }
    else if (dir === 'E') { jNew = j + 1 }
    
    if (!inGrid(iNew, jNew)) {
      break
    }
    if (!isBlocking(iNew, jNew)) {
      i = iNew
      j = jNew
    } else {
      if (dir === 'N') { dir = 'E' }
      else if (dir === 'S') { dir = 'W' }
      else if (dir === 'W') { dir = 'N' }
      else if (dir === 'E') { dir = 'S' }
    }
    
    if (directedVisited.has(intoDirectedKey(i, j, dir))) {
      return {
        uniques: visited.size, 
        looping: true,
        part2answer: part2answer,
      }
    }
  }
  
  // brute force part 2 cuz javascript is too annoying
  if (doPart2) {
    visited.forEach((k) => { 
      let [i, j] = outofKey(k)
      if (grid[i][j] === '.') {      
        grid[i][j] = '#'
        if (trace(grid, coordInit, false).looping) {
          part2answer += 1
        }
        grid[i][j] = '.'
      }
    })
  }
  
  return {
    uniques: visited.size,
    looping: false,
    part2answer: part2answer,
  }
}

function solve(input_file: string) {
  const input_text = fs.readFileSync(input_file, 'utf8')
  const grid = parse(input_text)
  const coordInit = locateGuard(grid)
  const solution = trace(grid, coordInit, true)
  console.log("part1", input_file, "solution:", solution)
}

solve('test.txt')
solve('input.txt')