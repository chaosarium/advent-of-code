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

def solve(grid: list[list[str]]):
    
    # make graph
    V = set()
    E = {}
    Erev = {} # in edges sets
    W = {}
    start = (-1, -1)
    end = (-1, -1)
    for i, row in enumerate(grid):
        for j, cell in enumerate(row):
            for dir in ["N", "S", "W", "E"]:
                E[(i, j, dir)] = set()
                Erev[(i, j, dir)] = set()

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
                
                # move
                iLandIfMove, jLandIfMove = movedCoord(i, j, dir)
                if grid[iLandIfMove][jLandIfMove] != "#":
                    E[(i, j, dir)].add((iLandIfMove, jLandIfMove, dir))
                    Erev[(iLandIfMove, jLandIfMove, dir)].add((i, j, dir))
                    W[((i, j, dir), (iLandIfMove, jLandIfMove, dir))] = 1
                
                # turn
                for newDir in ["N", "S", "W", "E"]:
                    if newDir == dir: continue
                    E[(i, j, dir)].add((i, j, newDir))
                    Erev[(i, j, newDir)].add((i, j, dir))
                    W[((i, j, dir), (i, j, newDir))] = turnCost(dir, newDir)
    
    # dijkstra
    
    hq = []
    initial_v = (start[0], start[1], "E")
    heappush(hq, (0, initial_v))
    visited = set([initial_v])
    part1answer = 9999999999
    part2answer = 0
    dists = { initial_v: 0 }
    
    while True:
        if len(hq) == 0: break
        # visited ... | u 
        # u -> v
        (dist_u, u) = heappop(hq)
        if not u in visited: 
            visited.add(u)
            dists[u] = dist_u
        if u[0] == end[0] and u[1] == end[1]:
            if dist_u < part1answer:
                print("FOUND")
                part1answer = dist_u
        for v in E[u]:
            if v in visited: continue
            w_uv = W[(u, v)]
            heappush(hq, (dist_u + w_uv, v))
    
    
    # recover part 2 answer
    
    tiles_on_sp = set()
    
    # find tiles on way to v via dist distance
    def retrace(v, dist_v):
        v_coord = (v[0], v[1])
        if v == initial_v:
            if dist_v == 0:
                tiles_on_sp.add(v_coord)
                return
            else:
                raise Exception ("what?")
                
        # u -> v
        for u in Erev[v]:
            u_coord = (u[0], u[1])
            w_uv = W[(u, v)]
            if dists[u] + w_uv != dist_v: 
                continue
            tiles_on_sp.add(u_coord)
            tiles_on_sp.add(v_coord)
            retrace(u, dist_v - w_uv)
        
    _ = [retrace((end[0], end[1], dir), part1answer) for dir in ["N", "S", "W", "E"]]
    part2answer = len(tiles_on_sp)
    
    return part1answer, part2answer

print("test (p1, p2)", solve(parse("test.txt")))
print("test2 (p1, p2)", solve(parse("test2.txt")))
print("real (p1, p2)", solve(parse("input.txt")))