#!/usr/bin/env python3.6
# vim: set foldmethod=marker:

import sys
import warnings
import numpy          as np
import networkx       as nx
import collections
import cplex
import subprocess
import time

from flow_capacity import get_edge_capacities
from utils         import *
from tests         import get_test

class AssumptionSolver:
    def __init__(self, g, must_eq, cannot_be_eq):
        self.g            = g
        self.must_eq      = list(set(must_eq))
        self.cannot_be_eq = list(set(cannot_be_eq))
        self.cap          = get_edge_capacities(g)
        self.shadow_nodes = [v for v in g.nodes() if len(g.succ[v]) > 0]

        # give ids to the edges
        n = 0
        edge_id = {}
        for e in g.edges():
            edge_id[e] = n
            n += 1
        for v in self.shadow_nodes:
            edge_id[v] = n
            n += 1
        self.edge_id    = edge_id
        self.edge_count = len(edge_id)

        # calculate costs of the nodes
        self.calc_costs()


    def calc_costs(self):
        """
        Returns a mapping from node ids to their costs
        """
        roots      = [v for v in self.g.nodes() if len(self.g.pred[v]) == 0]
        self.costs = collections.defaultdict(int)
        worklist   = collections.deque(roots)
        done       = set(roots)

        while worklist:
            v = worklist.popleft()
            c = sum(map(lambda u: self.costs[u], self.g.predecessors(v))) + 1
            self.costs[v] = c
            for u in self.g.successors(v):
                if u not in done:
                    worklist.append(u)
                    done.add(u)

class CplexAssumptionSolver(AssumptionSolver):
    def __init__(self, g, must_eq, cannot_be_eq):
        super(CplexAssumptionSolver, self).__init__(g, must_eq, cannot_be_eq)

    def suggest_assumptions(self):
        prob = cplex.Cplex()
        prob.set_results_stream(None)
        prob.set_log_stream(None)

        # objective is to minimize
        prob.objective.set_sense(prob.objective.sense.minimize)

        t0 = time.perf_counter()

        # compute objective coefficients
        obj = [0] * self.edge_count
        for v in self.shadow_nodes:
            obj[self.edge_id[v]] = self.costs[v]
        for v in self.cannot_be_eq:
            for w in self.g.successors(v):
                for u in self.g.predecessors(w):
                    obj[self.edge_id[u]] = self.costs[w] + 1

        # compute upper bounds on variables
        ub = [0] * self.edge_count
        for e,i in self.edge_id.items():
            if type(e) == int:
                v = e           # this is a shadow node
                ub[i] = sum(map(lambda w: self.cap[v,w], self.g.successors(v)))
            else:
                ub[i] = self.cap[e]

        # update cost and upper bound
        prob.variables.add(obj = obj, ub = ub)

        # the variables of nodes that cannot be marked has to be zero
        indices1 = [ self.edge_id[v]
                     for v in self.cannot_be_eq
                     if len(self.g.succ[v]) > 0 ]
        prob.linear_constraints.add(lin_expr = [[indices1, [1] * len(indices1)]],
                                    senses   = "E",
                                    rhs      = [0])

        # the outgoing flow of must_eq nodes have to be at capacity
        indices2 = [ self.edge_id[v,w]
                     for v in self.must_eq
                     for w in self.g.successors(v) ]
        prob.linear_constraints.add(lin_expr = [[indices2, [1] * len(indices2)]],
                                    senses   = "E",
                                    rhs      = [ sum([ ub[self.edge_id[v]]
                                                       for v in self.must_eq ]) ])

        # regular max flow constraints
        for v in self.g.nodes():
            if len(self.g.succ[v]) == 0:
                continue
            indices      = [self.edge_id[v]]
            coefficients = [-1]
            for u in self.g.predecessors(v):
                indices.append(self.edge_id[u,v])
                coefficients.append(-1)
            for w in self.g.successors(v):
                indices.append(self.edge_id[v,w])
                coefficients.append(1)
            prob.linear_constraints.add(lin_expr = [[indices, coefficients]],
                                        senses   = "L",
                                        rhs      = [0])

        prob.solve()
        sol = prob.solution
        status = sol.get_status_string()

        t1 = time.perf_counter()
        print("elapsed time: {} ms".format(int((t1-t0) * 1000)))

        prob.write("assumptions.lp")
        if status == "optimal":
            values = sol.get_values()
            return {v : values[self.edge_id[v]] for v in self.shadow_nodes}
        else:
            print("linprog failed: {}".format(status))
            sys.exit(1)

def suggest_assumptions(g, must_eq, cannot_be_eq):
    defaultSolver = CplexAssumptionSolver(g, must_eq, cannot_be_eq)
    return defaultSolver.suggest_assumptions()

def main2(test_no):
    test = get_test(test_no)
    if test is None:
        return

    edges, must_eq, cannot_be_eq = test
    g = make_test_graph(edges)
    result = suggest_assumptions(g, must_eq, cannot_be_eq)
    if result:
        print("Marked nodes:\n{}".format([v for v,n in result.items() if int(round(n)) > 0]))
        return result
    else:
        print("No solution exists...")

def main(filename):
    parsed       = parse_cplex_input(filename)
    g            = parsed["graph"]
    must_eq      = parsed["must_eq"]
    cannot_be_eq = parsed["cannot_be_eq"]
    names        = parsed["names"]

    def l2s(l):
        return ", ".join(l)

    write_dot_file(g, names)
    # visualize_graph()

    result = suggest_assumptions(g, must_eq, cannot_be_eq)
    if result:
        print("Must equal   :")
        for v in sorted([names[v] for v in must_eq]):
            print("  {}".format(v))
        print("Marked nodes : {}".format(l2s([names[v] for v,n in result.items() if round(n) > 0])))
    else:
        print("No solution exists...")
    return result

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("usage: assumptions.py <cplex.json>")
        sys.exit(1)
    else:
        main(sys.argv[1])