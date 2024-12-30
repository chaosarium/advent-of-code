import bisect 

def parse(input_path: str) -> list[tuple[int, int]]:
    lines = open(input_path).read().split("\n")
    lines = filter(lambda line: line != "", lines)
    edges = []
    for line in lines:
        u, v = line.split("-")[0], line.split("-")[1]
        edges.append((u, v))
        edges.append((v, u))
    return edges
    
def part1(edges: list[tuple[int, int]]) -> int:
    
    edges_set = set(edges)
    
    nbors = {}
    for (u, v) in edges:
        if u not in nbors:
            nbors[u] = set()
        nbors[u].add(v)
    
    triples = set()
    for u, u_N in nbors.items():
        for v1 in u_N:
            for v2 in u_N:
                if v1 == v2: continue
                if (v1, v2) in edges_set:
                    triple = tuple(sorted([u, v1, v2]))
                    triples.add(triple)
    
    count = 0
    for (a, b, c)in triples:
        if a[0] == 't' or b[0] == 't' or c[0] == 't':
            count += 1
    
    return count

def part2(edges: list[tuple[int, int]]) -> str:
    
    edges_set = set(edges)

    nbors = {}
    vertices = set()
    for (u, v) in edges:
        vertices.add(u)
        if u not in nbors:
            nbors[u] = set()
        nbors[u].add(v)
    
    # returns a list of unique cliques
    # subprob_sol is cache for recursive call, if available
    def find_k_cliques(k, subprob_sol = None):
        if k < 3:
            raise ValueError("not supported")
        elif k == 3:
            triples = set()
            for u, u_N in nbors.items():
                for v1 in u_N:
                    for v2 in u_N:
                        if v1 == v2: continue
                        if (v1, v2) in edges_set:
                            triple = tuple(sorted([u, v1, v2]))
                            triples.add(triple)
            return list(triples)
        else:
            if subprob_sol is None:
                subprob_sol = find_k_cliques(k-1)
            bigger_cliques_set = set()

            for smaller_clique_tup in subprob_sol:
                smaller_clique_list = list(smaller_clique_tup)
                smaller_clique_set = set(smaller_clique_list)
                for v in vertices:
                    if v in smaller_clique_set: continue
                    v_possible = True
                    for u in smaller_clique_list:
                        if (u, v) not in edges_set:
                            v_possible = False
                    if v_possible:
                        bigger_clique_list = [x for x in smaller_clique_list]
                        bisect.insort(bigger_clique_list, v) 
                        bigger_clique_tup = tuple(bigger_clique_list)
                        bigger_cliques_set.add(bigger_clique_tup)
            
            return list(bigger_cliques_set)
    
    k = 3
    curr_k_cliques = find_k_cliques(k)
    while True:
        k_plus_1_cliques = find_k_cliques(k+1, curr_k_cliques)
        if len(k_plus_1_cliques) == 0:
            break
        else:
            curr_k_cliques = k_plus_1_cliques
            k += 1
            print("concluding k >=", k)
    
    return ",".join(curr_k_cliques[0])

print("p1 test:", part1(parse("test.txt")))
print("p1 real:", part1(parse("input.txt")))
print("p2 test:", part2(parse("test.txt")))
print("p2 real:", part2(parse("input.txt")))