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

function trace(grid: string[][], coordInit: [number, number]) {
  var i = coordInit[0]
  var j = coordInit[1]
  var dir = 'N'
  var visited = new Set()
  
  const height = grid.length
  const width = grid[0].length
  
  function inGrid(i: number, j: number): boolean {
    return 0 <= i && i < height && 0 <= j && j < width
  }
  function isBlocking(i: number, j: number): boolean {
    return grid[i][j] === '#'
  }
  
  while (true) {
    visited.add(i * width + j)
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
  }
  
  return visited.size
}

function part1(input_file: string) {
  const input_text = fs.readFileSync(input_file, 'utf8')
  const grid = parse(input_text)
  const coordInit = locateGuard(grid)
  const solution = trace(grid, coordInit)
  console.log("part1", input_file, "solution:", solution)
}

part1('test.txt')
part1('input.txt')