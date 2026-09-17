# EMEWS Workflow: GA Calibration of the MDSE Microsimulation

This directory contains an [EMEWS](https://emews.org) (Extreme-scale Model Exploration with Swift)
workflow that calibrates the MDSE microsimulation
model implemented in `../R/` using a parallel **genetic algorithm (GA)**. The GA searches the
parameter space defined in `R/02_model_inputs.R` and maximizes the goodness-of-fit (`f_gof`)
of the simulation outputs against calibration targets.

The workflow targets the **Improv** HPC cluster (PBS scheduler) at Argonne / LCRC. It uses
Swift/T as the workflow engine, EQ/Py to embed a Python DEAP-based GA inside Swift/T as a
*resident task*, and per-individual model evaluations are run as Bash subprocesses that invoke
Rscript on the existing MDSE R code.

> **Looking for the parameter sweep?** This directory also holds a second, separate
> workflow that runs the model over a fixed list of parameter sets and policy
> scenarios — one run per line of a UPF — and saves the full outputs rather than a
> goodness-of-fit score. See [`docs/sweep-workflow.md`](docs/sweep-workflow.md).
> The two workflows share the `launch.py` / Bash / Rscript plumbing but have their
> own launcher, Swift script and R entry point.

---

## 1. High-level architecture

```
                ┌───────────────────────────────────────────────────────────┐
                │                       swift-t (MPI)                       │
                │                                                           │
   HPC job ───► │  ┌──────────────┐     gof      ┌──────────────────────┐   │
                │  │ ga.swift     │ ───────────► │ python/ga.py (DEAP)  │   │
                │  │ (workflow)   │              │   resident task      │   │
                │  │              │ ◄─ params ── │   via EQ-Py          │   │
                │  └──────┬───────┘              └──────────────────────┘   │
                │         │ obj() per individual                            │
                │         ▼                                                 │
                │  ┌────────────────────────────────────────────────────┐   │
                │  │ python/launch.py  →  scripts/run_mdse_microsim.sh  │   │
                │  │                   →  R/run_model.R                 │   │
                │  │                   →  ../R/03_model_functions.R     │   │
                │  │                       f_gof(params) → gof          │   │
                │  └────────────────────────────────────────────────────┘   │
                └───────────────────────────────────────────────────────────┘
```

Key idea: a single Swift/T job allocates many MPI ranks. One rank hosts the Python GA
(`ga.py`) as a resident task. The remaining ranks act as workers that, for each individual in
the GA population, fork an Rscript process that runs one MDSE simulation and reports back a
goodness-of-fit value.

---

## 2. Directory layout

Files marked **[sweep]** belong to the sweep workflow rather than the GA; everything
unmarked is the GA, and `launch.py` / `emews_utils.sh` / `EQ-Py` are shared.

```
emews/
├── README.md                         ← this file
├── docs/
│   └── sweep-workflow.md             ← [sweep] how to configure and run the sweep
├── data/
│   ├── cfgs/improv_ga.cfg            ← experiment / GA configuration
│   ├── cfgs/local_sweep.cfg          ← [sweep] experiment / sweep configuration
│   └── upfs/                         ← [sweep] UPF inputs (gitignored — generated, can be large)
├── etc/
│   ├── emews_utils.sh                ← log_script, check_directory_exists helpers
│   └── algo_params_utils.sh          ← (unused here) param-file rewriter
├── ext/
│   ├── EQ-Py/                        ← vendored EMEWS EQ/Py bridge (eqpy.py + EQPy.swift)
│   └── emews/emews.swift             ← vendored EMEWS Swift/T library (parse_json_list)
├── python/
│   ├── ga.py                         ← DEAP GA driver (resident task)
│   ├── ga_algorithms.py              ← eaMuPlusLambda + eaSimple (with checkpoint/log hooks)
│   └── launch.py                     ← run() for the GA, run_sweep() for the sweep
├── R/
│   ├── create_init_pop.R             ← writes calib_inputs.csv (initial bounds + names)
│   ├── run_model.R                   ← per-evaluation R entry point (f_gof → one scalar)
│   ├── run_model_sweep.R             ← [sweep] per-run R entry point (main → one .RData)
│   └── test/                         ← diagnostics (policy_arithmetic_check.R and its write-up)
├── scripts/
│   ├── improv_env.sh                 ← module loads & env for the Improv cluster
│   ├── local_env.sh                  ← [sweep] leaves conda so the system R is used
│   ├── run_mdse_microsim.sh          ← Bash wrapper around Rscript run_model.R
│   └── run_mdse_microsim_sweep.sh    ← [sweep] Bash wrapper around Rscript run_model_sweep.R
└── swift/
    ├── ga.swift                      ← Swift/T workflow
    ├── improv_run_ga.sh              ← submission script (sets env, generates ga_cfg.yaml, calls swift-t)
    ├── sweep.swift                   ← [sweep] Swift/T workflow: one obj() per UPF line
    └── local_run_sweep.sh            ← [sweep] launcher (sets env, copies inputs, calls swift-t)
```

At runtime, an experiment directory is created at `emews/experiments/<EXPID>/` containing:

- `cfg.cfg`                           — copy of the cfg file used
- `ga_cfg.yaml`                       — GA parameters generated from the cfg
- `calib_inputs.csv`                  — parameter bounds + names produced by `create_init_pop.R`
- `restart.pkl`                       — (optional) copy of the restart pickle if restarting
                                        from a previous generation
- `run_mdse_microsim.sh`              — copy of the per-eval Bash wrapper
- `tmp/<gen>_<idx>_result.txt`        — temporary per-eval result files (created then deleted)
- `results/result_<gen>.csv`          — per-generation: each individual + its GOF
- `<gen>_GA.pkl`                      — per-generation checkpoint (population, hall of fame, logbook)
- `initial_population.csv`            — initial population + fitness
- `improv_run_ga.sh.log`              — captured environment + script

`experiments/` and `results/` are gitignored.

Here <gen> refers to the generation (iteration) of the GA.

---

## 3. End-to-end execution flow

1. **Submit** — the user runs `swift/improv_run_ga.sh <EXPID> data/cfgs/improv_ga.cfg`.
   The script sources the cfg, sets `TURBINE_OUTPUT` / `EMEWS_PROJECT_ROOT` / `PYTHONPATH`
   environment variables, generates
   `ga_cfg.yaml` from the cfg, and submits a PBS job via `swift-t -m pbs`.

When the job executes, the following occurs:

1. **Pre-launch** — Swift/T's `TURBINE_PRELAUNCH` runs `R/create_init_pop.R`, which sources
   `../R/02_model_inputs.R` and writes `calib_inputs.csv` containing one row per parameter
   flagged with `calib == 1` (columns: `value, lower, upper, calib, name`). This is the source
   for parameter names and bounds used by the GA.

2. **Workflow startup** — `ga.swift` `main()` calls `start()` on the resident-work rank. EQ/Py
   initializes the `ga` package (i.e. imports `ga.py` and calls `ga.run()`), then hands the path
   of `ga_cfg.yaml` to Python via `EQPy_put`.

3. **Initial population** — `ga.py`:
   - reads `ga_cfg.yaml` (popsize, ngen, mu, lambda_, cxpb, mutpb, eta, indpb, hof, seed, restart_from),
   - reads `calib_inputs.csv` for `lower`, `upper`, `name` per parameter,
   - either samples `popsize` individuals uniformly from `[lower, upper]`, or loads `restart.pkl`
     if resuming.
   - configures DEAP (`cxSimulatedBinaryBounded`, `mutPolynomialBounded`, `selTournament(tournsize=2)`),
   - runs `eaMuPlusLambda` from `ga_algorithms.py`.

4. **Generation loop** — for each generation DEAP calls `queue_map` in `ga.py`, which:
   - serializes the population (the input parameters) as a `;`-separated string of JSON `{name: value}` dicts,
   - sends it to Swift via `eqpy.OUT_put`,
   - blocks on `eqpy.IN_get()` for the `;`-joined GOF results,
   - writes `results/result_<gen>.csv` (one row per individual, columns = param names + `gof`).

5. **Parallel evaluation** — back in Swift, `loop()` receives the params string, splits it,
   and `foreach`-launches one `obj()` per individual. `obj()` builds a small Python snippet that
   calls `launch.run(...)` (`python/launch.py`), executed via `python_persist`.

6. **One model run** — `launch.run()` calls `scripts/run_mdse_microsim.sh` (a copy of which
   lives in the experiment dir) as a subprocess, passing gender, end year, num persons, a
   per-run result file path, the JSON params line, and the path to `../` (the MDSE R repo root).
   The Bash wrapper sources `${SITE}_env.sh` (e.g. `improv_env.sh`), sets `OMP_NUM_THREADS=1` /
   `MKL_THREADS=1`, then runs `Rscript emews/R/run_model.R …`.

7. **R evaluation** — `run_model.R`:
   - sources `01_environment.R`, `02_model_inputs.R`, `03_model_functions.R`,
   - opens a `FORK` cluster of `MDSE_NUM_CORES` workers on a random free port,
   - parses the JSON parameter line, calls `f_gof(input_params)`,
   - writes the scalar GOF to the result file and stops the cluster.

8. **Result return** — the Python snippet (see step. 5) reads the result file, deletes it, and returns the
   GOF string to Swift. Swift joins all per-individual GOFs and ships them back to Python via
   `EQPy_put`. DEAP assigns these as fitness values and continues.

9. **Checkpointing** — at the end of each generation `eaMuPlusLambda` pickles
    `[population, halloffame, logbook]` to `<gen>_GA.pkl` in the experiment dir.

10. **Termination** — after `ngen` generations `ga.py` sends `"DONE"`; Swift breaks the loop
    and the job ends.

---

## 4. Configuration

All knobs live in `data/cfgs/improv_ga.cfg`. The submission script sources this file using bash.

### HPC / Swift-T

| Variable | Meaning |
|---|---|
| `CFG_WALLTIME`     | hpc scheduler walltime, e.g. `06:00:00` |
| `NODES`            | Number of compute nodes |
| `CFG_PPN`          | MPI ranks per node |
| `CFG_PROCS`        | Total MPI ranks (`NODES * CFG_PPN`) |
| `CFG_QUEUE`        | hpc scheduler queue (e.g. `compute`) |
| `CFG_PROJECT`      | hpc scheduler project / allocation |
| `CFG_PROCS_PER_RUN`| Cores passed to the R `FORK` cluster (`MDSE_NUM_CORES`) inside one model evaluation |

### Model

| Variable | Meaning |
|---|---|
| `CFG_NUM_PERSONS`  | Simulated individuals per cohort (`n.i`) |
| `CFG_GENDER`       | `females` or `males` — picks the parameter set in `02_model_inputs.R` and the precomputed data files |
| `CFG_END_YEAR`     | Final simulation year |

### Genetic algorithm (DEAP)

| Variable | Meaning |
|---|---|
| `CFG_POP_SIZE`     | Population size |
| `CFG_NGEN`         | Number of generations |
| `CFG_MU`           | Parents kept per generation (defaults to `CFG_POP_SIZE`) |
| `CFG_LAMBDA`       | Offspring per generation (defaults to `CFG_POP_SIZE`) |
| `CFG_CXPB`         | Crossover probability |
| `CFG_MUTPB`        | Mutation probability |
| `CFG_ETA`          | Crowding degree for SBX crossover and polynomial mutation |
| `CFG_INDPB`        | Per-attribute mutation probability |
| `CFG_HOF`          | Hall-of-fame size |
| `CFG_INIT_POP_SEED`| Seed for the initial uniform sample (passed via `ga_cfg.yaml`'s `seed` field; defaults to 42 if absent) |
| `CFG_RESTART_FROM` | Path to a previous `<gen>_GA.pkl` to resume from; leave empty to start fresh |


### Calibration parameters

The set of calibrated parameters and their bounds are defined in `../R/02_model_inputs.R` in
the `m.calib_inputs` matrix. The 4th column (`calib`) is the toggle — set it to `1` to include
a parameter in the GA search, `0` to hold it fixed at column 1's value. `create_init_pop.R`
exports only the `calib == 1` rows to `calib_inputs.csv`, and `ga.py` only sees those.

---

## 5. Running the workflow

### Prerequisites (on Improv)

- `swift-t` built with Python and MPI support, on `PATH`.
- A Python environment with `deap`, `numpy`, `pandas`, `pyyaml` (matching the Python in
  `improv_env.sh`).
- R 4.5.1 + the package set in `01_environment.R` installed under `R_LIBS_USER`
  (`/lcrc/project/EMEWS/improv/rlibs/4.5` in the current `improv_env.sh`).
- The MDSE precomputed input `.RData` files under `../data/` for the chosen gender.

### Edit the config

Open `data/cfgs/improv_ga.cfg` and set at least:

- `NODES`, `CFG_PPN`, `CFG_WALLTIME`, `CFG_QUEUE`, `CFG_PROJECT`
- `CFG_NUM_PERSONS`, `CFG_GENDER`, `CFG_END_YEAR`
- `CFG_POP_SIZE`, `CFG_NGEN`, and the other GA knobs
- `CFG_PROCS_PER_RUN` — must satisfy `CFG_PROCS_PER_RUN ≤ CFG_PPN` and leave enough ranks
  for ADLB servers and the resident GA rank.
- `CFG_RESTART_FROM` — leave empty for a fresh run, or point to a previous `<gen>_GA.pkl`.

Optionally toggle which parameters are calibrated by editing the 4th column of `m.calib_inputs`
in `../R/02_model_inputs.R`.

### Submit

```bash
cd emews
./swift/improv_run_ga.sh <EXPID> data/cfgs/improv_ga.cfg
```

`<EXPID>` becomes the experiment directory name under `emews/experiments/`. If that directory
already exists you'll be prompted (`check_directory_exists` in `etc/emews_utils.sh`) to confirm —
re-using an EXPID will write into the existing folder.

The submission script will:

1. Echo the resolved configuration.
2. Set Swift/T environment variables (`TURBINE_OUTPUT`, `TURBINE_IMPROV`, `MACHINE=pbs`,
   `LD_PRELOAD=libmpi.so`, etc.).
3. Create `experiments/<EXPID>/{tmp,results}`.
4. Copy the cfg and per-eval wrapper into the experiment dir.
5. Generate `ga_cfg.yaml` from the cfg.
6. Submit the PBS job via `swift-t -m pbs`.

### Monitor

- PBS: `qstat -u $USER`, look for the job named `<EXPID>_job`.
- Per-generation results: tail `experiments/<EXPID>/results/result_<gen>.csv`.
- Standard out / err from Swift: the files placed by Turbine under the experiment dir
  (`TURBINE_STDOUT` is unset → the default `output.txt`).
- Failed individual evaluations: `experiments/<EXPID>/tmp/<gen>_<idx>_{out,err}.txt` (only
  written when `Rscript` exits non-zero — see `launch.py`).

### Restart from a checkpoint

Set `CFG_RESTART_FROM` to a checkpoint pickle from a prior run:

```bash
CFG_RESTART_FROM=X/experiments/<old_EXPID>/N_GA.pkl
```

where `X` is the prefix path to the `experiments` directory, and `N` is the generation.

`improv_run_ga.sh` copies pkl file to `experiments/<new_EXPID>/restart.pkl` and `ga.py` loads
`population` from it instead of sampling a fresh one. `ngen` then runs from the restarted
population. (`hof` and `logbook` from the pickle are not restored.)

---

## 6. Outputs and how to consume them

Per generation `g`:

- `results/result_<g>.csv` — one row per individual evaluated in that generation. Columns are
  the calibrated parameter names in the order they appear in `calib_inputs.csv`, plus `gof`.
  The first row (header) names the columns.
- `<g>_GA.pkl` — pickled `[population, halloffame, logbook]`. Load with:

  ```python
  import pickle
  with open("10_GA.pkl", "rb") as f:
      population, hof, logbook = pickle.load(f)
  best = max(hof, key=lambda ind: ind.fitness.values[0])
  ```

The best parameter vector can then be plugged back into the standard R driver scripts
(`04_calibration.R` / `05_validation.R`) by editing the `m.calib_inputs` `value` column or
by passing it directly to `f_gof` / `main_calib`.

---

## 7. Notes

- **Resident task ranks**: `improv_run_ga.sh` sets `ADLB_SERVERS=1`, `TURBINE_RESIDENT_WORK_WORKERS=1`,
  and `RESIDENT_WORK_RANKS=PROCS-2`. The resident-work rank is the last MPI rank; that's the
  one hosting `ga.py`. The remaining ranks are workers.
- **`CFG_PROCS_PER_RUN`**: each model evaluation spawns its own R `FORK` cluster of this size.
  Make sure `CFG_PPN ≥ CFG_PROCS_PER_RUN` and that your total concurrency
  (`(CFG_PROCS-2) * CFG_PROCS_PER_RUN`) fits within the node's cores.
- **Hard-coded Python in `R/run_model.R`**: the call to find a free port uses an absolute path
  to a specific Python install on Improv. Update it if you port to another cluster.
- **`SITE=improv`**: `scripts/run_mdse_microsim.sh` sources `${SITE}_env.sh`. To target another
  cluster, add a `scripts/<site>_env.sh`, a `swift/<site>_run_ga.sh`, and a
  `data/cfgs/<site>_ga.cfg`, and set `SITE` accordingly.
- **Stale checkpoint path**: `CFG_RESTART_FROM` may point to an existing
  run on Improv. Clear it (or replace it) before starting fresh experiments.
- **Invalid parameter combinations**: `f_gof` may return `INVALID_PROBS_FLAG` (defined in
  `03_model_functions.R`) when probabilities go out of range. These values flow back into the
  GA as fitnesses; the GA will naturally select them out, but you may want to inspect
  how often this occurs in `result_<g>.csv`.
