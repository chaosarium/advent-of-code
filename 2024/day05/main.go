// turns out not an elegant solution 💀
// advice to futere self: read problem before writing code

package main

import (
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"

)

type rule struct {
	page  int
	dependsOn int
}

type Problem struct {
	deps    []rule
	updates [][]int
}

func str2int(text string) int {
	res, _ := strconv.Atoi(text)
	return res
}

func str2intarr(text string) []int {
	res := make([]int, 0)
	for _, str := range strings.Split(text, ",") {
		res = append(res, str2int(str))
	}
	return res
}

func parse(file_path string) Problem {
	file, err := os.Open(file_path)
	if err != nil { panic(err) }
	defer file.Close()
	
	input_bytes, err := io.ReadAll(file)
	input_text := string(input_bytes)
	rules_text := strings.Split(input_text, "\n\n")[0]
	updates_text := strings.Split(input_text, "\n\n")[1]
	
	prob := Problem{
		deps: make([]rule, 0),
		updates: make([][]int, 0),
	}
	
	for _, line := range strings.Split(rules_text, "\n") {
		page := str2int(strings.Split(line, "|")[1])
		dependency := str2int(strings.Split(line, "|")[0])
		prob.deps = append(prob.deps, rule{page: page, dependsOn: dependency})
	}
	
	for _, line := range strings.Split(updates_text, "\n") {
		if line == "" { continue }
		updates := str2intarr(line)
		prob.updates = append(prob.updates, updates)
	}
	
	return prob
}

type unit struct{}

type Graph struct {
	outs map[int](map[int]unit)
}

// reachable from root via allowed vertices
func reach(g Graph, root int, allowed *map[int]unit, reachable map[int]unit) {
	// root -> thing root depends on
	if g.outs[root] == nil { return }
	for v, _ := range g.outs[root] {
		_, allowedContainsv := (*allowed)[v]
		if !allowedContainsv {continue}
		_, reachableContainsv := reachable[v]
		if !reachableContainsv {
			reachable[v] = unit{}
			reach(g, v, allowed, reachable)
		}
	}
}

// whether A ⊆ B
func isSubsetOf(A map[int]unit, B map[int]unit) bool {
	for k, _ := range A {
		_, inB := B[k]
		if !inB {
			return false
		}
	}
	return true
}

func setMinus(A map[int]unit, B map[int]unit) map[int]unit {
	difference := make(map[int]unit)
	for k, _ := range A {
		_, inB := B[k]
		if !inB {
			difference[k] = unit{}
		}
	}
	return difference
}

func compute(prob Problem) (int, int) {
	
	// make graph
	
	g := Graph{
		outs: make(map[int](map[int]unit)),
	}
	
	for _, dep := range	prob.deps {
		// u -> v
		u := dep.page
		v := dep.dependsOn
		if g.outs[u] == nil {
			g.outs[u] = make(map[int]unit)
		}
		g.outs[u][v] = unit{}
	}
	
	// fmt.Printf("graph:\n%+v\n", g)
		
	part1acc := 0
	part2acc := 0
	for _, update := range prob.updates {
		
		// set of updated pages
		
		updated := make(map[int]unit)
		for _, x := range update {
			updated[x] = unit{}
		}
		
		// make dependency sets

		page2dependencies := make(map[int](map[int]unit))
		for root, _ := range g.outs { 
			page2dependencies[root] = make(map[int]unit)
			reach(g, root, &updated, page2dependencies[root])
		}

		satisfied := make(map[int]unit)
		wellOrderedAsIs := true // for part 2, whether original ordering is good
		for i, x := range update {
			dependencies := page2dependencies[x]
			if page2dependencies[x] != nil && !isSubsetOf(dependencies, satisfied) {
				wellOrderedAsIs = false // ordering is bad...
				
				// greedily fix ordering? (part 2)
				missingDependencies := setMinus(dependencies, satisfied)
				j := i+1
				for j < len(update) {
					_, inDifference := missingDependencies[update[j]]
					if inDifference && isSubsetOf(page2dependencies[update[j]], satisfied) {
						break
					}
					j++ 
				}
				tmp := update[i]
				update[i] = update[j]
				update[j] = tmp
			}
			satisfied[update[i]] = unit{}			
		}
		if wellOrderedAsIs {
			part1acc += update[len(update) / 2]
		} else {
			part2acc += update[len(update) / 2]	
		}
		
	}
	
	return part1acc, part2acc
}

func main() {
	p1, p2 := compute(parse("test.txt"))
	fmt.Printf("test p1: %d\n", p1)
	fmt.Printf("test p2: %d\n", p2)

	p1, p2 = compute(parse("input.txt"))
	fmt.Printf("real p1: %d\n", p1)
	fmt.Printf("real p2: %d\n", p2)	
}
