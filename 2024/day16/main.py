# surely this can be reduced to min cost max flow
# but let's do dijkstra instead :p

from heapq import heappop, heappush

def parse(input_file):
    lines = open(input_file).read().split("\n")
    filter(lambda line: line != "", lines)
    return [[c for c in line] for line in lines]

def movedCoord(i: int, j: int, dir: str):
    match dir:
        case "N": 
            return (i-1, j)
        case "S": 
            return (i+1, j)
        case "W": 
            return (i, j-1)
        case "E": 
            return (i, j+1)
    raise ValueError

def turnCost(dir1, dir2):
    angle1 = ["N", "E", "S", "W"].index(dir1) + 4
    angle2 = ["N", "E", "S", "W"].index(dir2) + 4
    if abs(angle1 - angle2) == 3:
        return 1000
    elif abs(angle1 - angle2) == 2:
        return 2000
    elif abs(angle1 - angle2) == 1:
        return 1000
    else:
        raise ValueError

def solve1(grid: list[list[str]]):
    
    # make graph
    V = set()
    E = {}
    W = {}
    start = (-1, -1)
    end = (-1, -1)
    for i, row in enumerate(grid):
        for j, cell in enumerate(row):
            if cell == "#":
                continue
            if cell == "S":
                start = (i, j)
            if cell == "E":
                end = (i, j)
            
            for dir in ["N", "S", "W", "E"]:
                V.add((i, j, dir))
                E[(i, j, dir)] = set()
                
                # move
                iLandIfMove, jLandIfMove = movedCoord(i, j, dir)
                if grid[iLandIfMove][jLandIfMove] != "#":
                    E[(i, j, dir)].add((iLandIfMove, jLandIfMove, dir))
                    W[((i, j, dir), (iLandIfMove, jLandIfMove, dir))] = 1
                
                # turn
                for newDir in ["N", "S", "W", "E"]:
                    if newDir == dir: continue
                    E[(i, j, dir)].add((i, j, newDir))
                    W[((i, j, dir), (i, j, newDir))] = turnCost(dir, newDir)
    
    # dijkstra
    
    hq = []
    initial_v = (start[0], start[1], "E")
    heappush(hq, (0, initial_v))
    visited = set([initial_v])
    
    while True:
        if len(hq) == 0: raise Exception("did we not find it?")
        # visited ... | u 
        # u -> v
        (dist_u, u) = heappop(hq)
        visited.add(u)
        if u[0] == end[0] and u[1] == end[1]:
            print("FOUND")
            return dist_u
        for v in E[u]:
            if v in visited: continue
            w_uv = W[(u, v)]
            heappush(hq, (dist_u + w_uv, v))
        

print("p1 test", solve1(parse("test.txt")))
print("p1 test2", solve1(parse("test2.txt")))
print("p1 real", solve1(parse("input.txt")))