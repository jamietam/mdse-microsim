from dataclasses import dataclass, field
from typing import List
import yaml
import random
import numpy as np
import pandas as pd
from deap import base, creator, tools
import json
import eqpy
import math
import pickle
import os
import csv

from ga_algorithms import eaMuPlusLambda


def get_individual_f(size: int):
    def f():
        return [x for x in range(size)]
    return f


def ts(x):
    from datetime import datetime
    return datetime.now().strftime("%Y-%m-%d %H:%M:%S")


@dataclass
class GAArgs:
    init_pop_path: str
    seed: int = 42
    ngen: int = 3
    popsize: int = 1000
    mutpb: float = 0.8
    cxpb: float = 0.8
    indpb: float = 0.25
    eta: float = 5.0
    lambda_: int = 1000
    mu: int = 1000
    hof: int = 50
    restart_from: str = None
    ub: List[float] = field(default_factory=list)
    lb: List[float] = field(default_factory=list)
    names: List[str] = field(default_factory=list)

iter = 1

def write_results(results: List, population, names):
    global iter
    f = os.path.join(os.environ["TURBINE_OUTPUT"], "results", f"result_{iter}.csv")
    with open(f, "w") as fout:
        writer = csv.writer(fout)
        writer.writerow(names + ["gof"])
        for i, indiv in enumerate(population):
            writer.writerow(indiv + [results[i][0]])
    iter += 1


def queue_map(obj_func, population, names):
    # Note that the obj_func is not used
    # sending data that looks like:
    # [[a,b,c,d],[e,f,g,h],...]
    if not population:
        return []
    # print(pops, flush=True)
    payload = ";".join([json.dumps({names[i]: val for i, val in enumerate(p)}) for p in population])  # json.dumps(population)
    # print(payload, flush=True)
    eqpy.OUT_put(payload)
    # print("WAITING for IN_get()", flush=True)
    result = eqpy.IN_get()
    # print("IN_get complete", flush=True)
    # print(result)
    split_result = result.split(';')
    # # TODO determine if max'ing or min'ing and use -9999999 or 99999999
    # return [(float(x),) if not math.isnan(float(x)) else (float(99999999),) for x in split_result]
    # return [(float(x),) for x in split_result]
    gofs = [(float(x),) for x in split_result]
    write_results(gofs, population, names)
    return gofs


def run_GA(args: GAArgs, jlpop):

    # print(args, flush=True)

    creator.create("FitnessMax", base.Fitness, weights=(1.0,))
    creator.create("Individual", list, fitness=creator.FitnessMax)

    toolbox = base.Toolbox()
    toolbox.register("map", queue_map, names=args.names)
    toolbox.register("evaluate", lambda x: 0)

    toolbox.register("individual", tools.initIterate, creator.Individual,
                     get_individual_f(len(jlpop[0])))
    toolbox.register("population", tools.initRepeat, list, toolbox.individual)
    toolbox.register("mate", tools.cxSimulatedBinaryBounded,
                     eta=args.eta, low=args.lb, up=args.ub)
    toolbox.register("mutate", tools.mutPolynomialBounded,
                     eta=args.eta, low=args.lb, up=args.ub, indpb=args.indpb)
    toolbox.register("select", tools.selTournament, tournsize=2)

    if args.restart_from is None:
        # update the deap population with values from jl pop
        population = toolbox.population(n=len(jlpop))
        for i, indiv in enumerate(jlpop):
            for j, val in enumerate(indiv):
                if val is None:
                    val = math.nan
                population[i][j] = val
    else:
        print(f"Restarting from: {args.restart_from}", flush=True)
        with open(args.restart_from, "rb") as fin:
            population = pickle.load(fin)[0]

    # print(population, flush=True)

    hall_of_fame = tools.HallOfFame(maxsize=args.hof)
    stats = tools.Statistics(key=lambda x: x.fitness.values)
    stats.register("avg", np.mean)
    stats.register("std", np.std)
    stats.register("min", np.min)
    stats.register("max", np.max)
    stats.register("ts", ts)

    # final_population, logbook = eaSimple(
    eaMuPlusLambda(
        population, toolbox,
        mu=args.mu,
        lambda_=args.lambda_,
        cxpb=args.cxpb, mutpb=args.mutpb,
        ngen=args.ngen,
        verbose=True,
        stats=stats,
        halloffame=hall_of_fame,
    )
    eqpy.OUT_put("DONE")


def run():
    # parse args from EQPy_In here
    eqpy.OUT_put("Params")
    params_f = eqpy.IN_get()
    # params_f = "/home/nick/Documents/repos/ENGAGE/emews/data/algo_params/ga_params.yaml"
    with open(params_f) as fin:
        params = yaml.safe_load(fin)

    args = GAArgs(**params)
    random.seed(args.seed)

    rng = np.random.default_rng(args.seed)
    inputs = pd.read_csv(args.init_pop_path)
    init_pop = [rng.uniform(inputs.lower, inputs.upper) for _ in range(args.popsize)]
    
    args.lb = list(inputs.lower)
    args.ub = list(inputs.upper)
    args.names = inputs.name.to_list()

    run_GA(args, init_pop)


if __name__ == '__main__':
    run()
