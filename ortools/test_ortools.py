from ortools.linear_solver import pywraplp

def print_matrix(matrix):
    for line in matrix:
        print(''.join([f'{x:3}' if x else '   ' for x in line]))

def solve(matrix, dV=1, debug=False):
    I = len(matrix)
    J = len(matrix[0])
    Vmin = max([max(l) for l in matrix])
    V = Vmin + dV
    Vrange = range(Vmin + 1, V + 1)

    solver = pywraplp.Solver.CreateSolver('SCIP')
    if not solver:
        if debug: print("Couldn't create solver")
        return

    vs = {}
    fs = []
    for i in range(I):
        for j in range(J):
            if matrix[i][j] == 0:
                fs.append((i, j))
                for v in Vrange:
                    vs[(v,i,j)] = solver.IntVar(0, 1, f'x{v}.{i}.{j}')
    if debug: print(f'Num vars: {solver.NumVariables()}')

    for v in Vrange:
        solver.Add(solver.Sum([vs[v,i,j] for (i,j) in fs]) <= 1)
    for (i,j) in fs:
        solver.Add(solver.Sum([vs[v,i,j] for v in Vrange]) <= 1)
    for v in Vrange[:-1]:
        solver.Add(solver.Sum([vs[v+1,i,j] for (i,j) in fs]) -
                solver.Sum([vs[v,i,j] for (i,j) in fs]) <= 0)
    for (i,j) in fs:
        neighs = [(i+di,j+dj)
                for di in [-1,0,1] for dj in [-1,0,1]
                if di != 0 or dj != 0
                if 0 <= i+di < I and 0 <= j+dj < J]
        fixed_sum = sum(matrix[x][y] for (x,y) in neighs)
        neighs = [(x,y) for (x,y) in neighs if matrix[x][y] == 0]
        for v in Vrange:
            l = [w * vs[w,x,y]
                 for w in Vrange if w < v
                 for (x,y) in neighs]
            solver.Add(v*vs[v,i,j] - solver.Sum(l) <= fixed_sum)
            solver.Add(v*vs[v,i,j] + solver.Sum(l) <= 2 * v - fixed_sum)
    if debug: print(f'Num constraints: {solver.NumConstraints()}')

    solver.Maximize(solver.Sum([vs[V,i,j] for (i,j) in fs]))

    if debug:
        print('='*80)
        print(solver.ExportModelAsLpFormat(False))
        with open("obj", "w") as f:
            f.write(solver.ExportModelAsLpFormat(False))
        print('='*80)

    status = solver.Solve()
    if status in [pywraplp.Solver.OPTIMAL, pywraplp.Solver.FEASIBLE]:
        def _p():
            if solver.Objective().Value():
                if debug:
                    print(f'z={solver.Objective().Value()}')
                    for v in vs.values():
                        print(f'{v}={v.solution_value()}')
                for v,i,j in vs:
                    if vs[v,i,j].solution_value():
                        matrix[i][j] = v
                print_matrix(matrix)
                for i,j in fs:
                    matrix[i][j] = 0
        _p()
        while solver.NextSolution():
            _p()
    else:
        print(f'status={status}')


matrix = [
 [0,0,0,0,0,0,0,0],
 [0,0,0,0,0,0,0,0],
 [0,0,3,1,1,5,0,0],
 [0,0,0,2,4,0,0,0],
 [0,0,0,0,0,0,0,0],
 [0,0,0,0,0,0,0,0],
]

solve(matrix, dV=3, debug=False)
