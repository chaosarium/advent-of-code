from ortools.linear_solver import pywraplp

eps1 = 0.01
eps2 = 0.0001

def lp_solve(
    aX: int,
    bX: int,
    aY: int,
    bY: int,
    prizeX: int,
    prizeY: int,
):
    solver = pywraplp.Solver.CreateSolver("SCIP")

    a = solver.IntVar(0.0, solver.infinity(), "a")
    b = solver.IntVar(0.0, solver.infinity(), "b")
    
    # solver.Add(a <= 100.001)
    # solver.Add(b <= 100.001)
    solver.Add(a * aX + b * bX == prizeX)
    solver.Add(a * aY + b * bY == prizeY)
    # solver.Add(a * aX + b * bX >= prizeX - eps2)
    # solver.Add(a * aX + b * bX <= prizeX + eps2)
    # solver.Add(a * aY + b * bY >= prizeY - eps2)
    # solver.Add(a * aY + b * bY <= prizeY + eps2)
    
    solver.Minimize(3 * a + 1 * b)
    
    status = solver.Solve()
    
    if not status == pywraplp.Solver.OPTIMAL:
        return None
        
    # aSol = a.solution_value()
    # bSol = b.solution_value()
    objVal = solver.Objective().Value()
    # if abs(round(aSol) - aSol) > eps1:
    #     return None
    # if abs(round(bSol) - bSol) > eps1:
    #     return None
    return round(objVal)

def brute_solve(
    aX: int,
    bX: int,
    aY: int,
    bY: int,
    prizeX: int,
    prizeY: int,
):
    best = 9999999999999
    for a in range(100):
        for b in range(100):
            if a * aX + b * bX == prizeX and a * aY + b * bY == prizeY:
                obj = 3 * a + 1 * b
                best = min(best, obj)
    if best == 9999999999999:
        return None
    return best

def solve_input(file_name, part2: bool, solver: str):
    with open(file_name, 'r') as f:
        input_str = f.read()
    
    acc = 0
    for prob_str in input_str.split('\n\n'):
        prob_lines = prob_str.split("\n")
        aX = int(prob_lines[0].split(",")[0].split("+")[-1])
        bX = int(prob_lines[1].split(",")[0].split("+")[-1])
        prizeX = int(prob_lines[2].split(",")[0].split("=")[-1])
        aY = int(prob_lines[0].split(",")[1].split("+")[-1])
        bY = int(prob_lines[1].split(",")[1].split("+")[-1])
        prizeY = int(prob_lines[2].split(",")[1].split("=")[-1])
        if part2:
            prizeX += 10000000000000
            prizeY += 10000000000000
        
        if solver == 'brute':
            sol = brute_solve(aX = aX, bX = bX, prizeX = prizeX, aY = aY, bY = bY, prizeY = prizeY)
        else:
            sol = lp_solve(aX = aX, bX = bX, prizeX = prizeX, aY = aY, bY = bY, prizeY = prizeY)
        if sol is not None:
            acc += sol
        
    return acc

print("PART 1")
print("test (brute):", solve_input("test.txt", part2=False, solver='brute'))
print("test (lp):", solve_input("test.txt", part2=False, solver='lp'))
print("real (brute):", solve_input("input.txt", part2=False, solver='brute'))
print("real (lp):", solve_input("input.txt", part2=False, solver='lp'))
print()
print("PART 2")
print("test (lp):", solve_input("test.txt", part2=True, solver='lp'))
print("real (lp):", solve_input("input.txt", part2=True, solver='lp'))
