# AGENTS.md

No package, build, lint, formatter, or test suite. Plain R scripts sourced in order.

## Sourcing contract (must follow this order)

Every driver must set these globals **before** sourcing:

```r
mainDir <- "/abs/path/to/mdse-microsim/"  # trailing slash required
args    <- c("females", 10000, 2100)       # c(gender, n.i, endyear) — positional, no flags
seednew <<- 1
s.HD_2100 <- 1
source(paste0(mainDir, "R/01_environment.R"))
source(paste0(mainDir, "R/02_model_inputs.R"))    # reads args[1..3] directly
source(paste0(mainDir, "R/03_model_functions.R"))
```

- `mainDir` is hard-coded to a contributor's local path in `R/04_calibration.R` and `R/06_analysis.R` — edit locally, don't commit.
- Switching gender requires re-sourcing both `02` and `03` (see `R/06_analysis.R:89-91`).
- Heavy use of `<<-` / lexical globals: `f_gof` reads `m.calib_inputs`, `l.calib_targets`, `n.target`, `calib_startyear`, `calib_splityear`, `endyear`, `seednew` from global env.

## Pipeline (numbered scripts = run order)

- `R/01`–`R/03` sourced, never standalone. `01` installs/loads packages (needs R 4.4.0+).
- `R/04_calibration.R` — L-BFGS-B + LHS local calibration; superseded on HPC by EMEWS GA. Sources `05` at end.
- `R/05_validation.R` — validation figures vs NSDUH targets.
- `R/06_analysis.R` — policy scenarios. `run_scenarios = 0` reloads `output/*.RData` instead of simulating; `=1` runs `run_policy` loop over both genders. Sources `07` at end.
- `R/06a`, `R/07`, `R/07a` — combined-gender results, figures, tables.
- `R/data_clean_*.R`, `R/*_precompute_inputs.R`, `R/*_calib_targets.R` — one-off generators for `data/*.RData`.

## Calibration parameters

`R/02_model_inputs.R:34-158` — `m.calib_inputs` columns = `value, lower, upper, calib`. Column 4 is the on/off toggle (`1`=searched, `0`=pinned). Male and female blocks have **different parameter sets**, not just different values. Access via `get_value(name, v.params)` — `v.params` contains only `calib==1` rows.

Changing toggles changes the GA search space and breaks restart compatibility with old `*_GA.pkl` checkpoints (bare float lists, no names).

## Key functions in `R/03_model_functions.R`

- `mds_microsim(bc)` / `probs()` / `main_calib(v.params)` / `main(v.params, ...)` / `f_gof(v.params)`
- `probs()` returns `INVALID_PROBS_FLAG` (`-999999999999999`) on bad params — check with `length(x)==1 && x==INVALID_PROBS_FLAG`, never bare `==` (success is a matrix).
- `f_gof` is **maximized** (sum of normal log-lik, years >=2020 weighted 2x). `04` passes `fnscale=-1` to `optim`.
- `main_calib` uses `foreach %dopar%` with explicit `.export` — new globals used inside the loop must be added to that vector (`R/03_model_functions.R:713`).
- Variable prefixes: `p.` prob, `s.` scale, `rr.` risk, `m.` matrix, `v.` vector, `l.` list, `n.` count.

## Health states

`v.n` = 28 states + `X` (dead). Code = `N/C/F` (smoking) + `O/E/Q` (e-cig) + `H/D/R` (depression). e.g. `NOH`=start, `CQD`=current smoker/quit vaper/depressed. `v.ysq` derived from `grepl("F", state)`, capped at 40.

## Data conventions

- Input files: `data/<domain>_precomputed_inputs_<gender>.RData` where `<gender>` is `males`/`females`. New gender-specific inputs must follow this pattern.
- Output filenames: `<seed>_<policyyear>rnc_<seed><gender>_depression_<n.i>_<timestamp>.RData`.

## Running

```bash
Rscript R/06_analysis.R                          # local — edit mainDir first
Rscript R/04_calibration.R males 1000 2022 25     # HPC/Slurm: gender n.i endyear n.init
```

## EMEWS GA (`emews/`)

See `emews/README.md`. Short version: `emews/swift/improv_run_ga.sh <EXPID> data/cfgs/improv_ga.cfg` submits PBS job. One MPI rank runs DEAP GA (`python/ga.py`) as EQ/Py resident task; workers fork `Rscript emews/R/run_model.R` -> `f_gof`. Results in `emews/experiments/<EXPID>/` (gitignored).

- `R/create_init_pop.R` exports `calib_inputs.csv` from `m.calib_inputs` — GA reads this for bounds/names.
- `CFG_RESTART_FROM` in `data/cfgs/improv_ga.cfg` may point at a stale checkpoint — clear for fresh runs.
- `emews/R/run_model.R:9` hard-codes an Improv Python path for free-port discovery — change when porting.
- `CFG_PROCS_PER_RUN` -> `MDSE_NUM_CORES` (R FORK cluster size). Total concurrency = `(CFG_PROCS-2)*CFG_PROCS_PER_RUN`.
- Site config is triple-coupled: `data/cfgs/<site>_ga.cfg` + `scripts/<site>_env.sh` + `swift/<site>_run_ga.sh`.

Sweep workflow (`emews/swift/sweep.swift`, `emews/scripts/run_mdse_microsim_sweep.sh`, etc.) is an emewscreator scaffold — still placeholder/stub. Mirror the GA path to fill it in.

## Workflow preferences (owner request)

- Ask before editing any file (use `question` tool) — do not edit unannounced.
- Make changes on a separate branch off the current branch by default (not directly on the current branch); docs-only changes to `AGENTS.md` are exempt.
- Commit without asking for confirmation once edits are approved.
