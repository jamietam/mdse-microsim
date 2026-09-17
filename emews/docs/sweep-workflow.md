# EMEWS Sweep Workflow

Runs the MDSE microsimulation over a list of parameter sets and over policy scenarios (optional),
one model run per line of a UPF (unrolled parameter file), each in parallel under
Swift/T. Every run produces a `.RData` file of full model outputs — prevalences,
costs, QALYs, productivity, deaths.

This is the batch equivalent of the `run_scenarios = 1` branch of
[`../../R/06_analysis.R`](../../R/06_analysis.R). It is a *different* workflow from the
GA calibration described in [`../README.md`](../README.md): the GA searches for
parameters and returns one goodness-of-fit number per evaluation, while the sweep
takes parameters as given and returns the full result set.

| | GA | Sweep |
|---|---|---|
| Launcher | `swift/improv_run_ga.sh` | `swift/local_run_sweep.sh` |
| Swift script | `swift/ga.swift` | `swift/sweep.swift` |
| Python entry | `launch.run()` | `launch.run_sweep()` |
| Bash wrapper | `scripts/run_mdse_microsim.sh` | `scripts/run_mdse_microsim_sweep.sh` |
| R entry | `R/run_model.R` → `03_model_functions.f_gof()` | `R/run_model_sweep.R` → `03_model_functions.main()` |
| What varies | chosen by the GA | fixed, one line per run |
| Output per run | one scalar | one `.RData` |

---

## 1. Running it

### Activate the local environment

Swift/T and its embedded Python come from a conda environment, installed
using the emews conda install instructions. Once installed, it can be activated
with:

```bash
conda activate <conda_env>
```

For example,

```bash
conda activate emews-py3.11
```


This provides `swift-t` etc. Nothing else in the sweep depends on
this environment's Python beyond the standard library — `launch.py` only uses
`os`, `subprocess`, `datetime`, `pathlib` and `stat`. (Note that the GA additionally needs
`deap`, `numpy`, `pandas` and `pyyaml`; the sweep does **not**.)

**The model itself does not run in this environment.** This is for convenience, as
we assumed that you already had an R set up with all the packages to run mdse and
there was no need to duplicate that. See
[R comes from outside conda](#r-comes-from-outside-conda) below for more details.

### Edit the two inputs

1. [`data/cfgs/local_sweep.cfg`](../data/cfgs/local_sweep.cfg) — settings shared by every run
2. The UPF named by `CFG_UPF` — **one row per model run**; the row count is the size
   of the sweep

Both are described in detail in §2 and §3.

### Launch

```bash
cd emews
./swift/local_run_sweep.sh <EXPID> data/cfgs/local_sweep.cfg
```

`<EXPID>` names the experiment directory created at `emews/experiments/<EXPID>/`.
If it already exists you are prompted to confirm before it is reused — that prompt
is the one interactive moment, and it happens before output is redirected.

### Watch it

With `MACHINE=""` (the default in `local_run_sweep.sh`, meaning an immediate
unscheduled run) the launcher redirects all subsequent stdout and stderr:

```bash
tail -f emews/experiments/<EXPID>/output.txt
```

Nothing further appears on your terminal, so this is the only way to see progress.
Per-run output lands in `tmp/<i>_out.txt` and `tmp/<i>_err.txt`.

---

## 2. `data/cfgs/local_sweep.cfg`

Format is plain environment file, `NAME=value` with **no** spaces around `=`.

The required variables are described below.

### Parallelism

| Variable | Meaning |
|---|---|
| `NODES` | Number of compute nodes |
| `CFG_PPN` | Number of runs per node |
| `CFG_PROCS` | Total number of runs, `$(( NODES * CFG_PPN ))` |
| `CFG_PROCS_PER_RUN` | Number of cores for the R `FORK` cluster **inside one model run** |

These work together as follows. A node typically has some number of cpus cores. We need to
allocate these such that the number of runs per node (`CFG_PPN`) * the number of cores allocated
within a run for the R doParallel cluster equals the number of cores that the node has. So,
for example, if the node has 8 cores, we could do 4 runs per node (`CFG_PPN`) and 2 cores per R doParallel (`CFG_PROCS_PER_RUN`) call.

`NODES` is typically 1 for a local run on a laptop where a typical laptop CPU may have between 8 and 16 cores. An HPC machine typically has 10s or 100s of nodes with 128 cores per node.

**Watch the total.** Swift/T reserves cores for ADLB servers, so roughly
`CFG_PROCS - 1` ranks run model evaluations, and each forks `CFG_PROCS_PER_RUN`
R workers. Peak processes is about `(CFG_PROCS - 1) × CFG_PROCS_PER_RUN`. The
committed defaults (`CFG_PROCS=4`, `CFG_PROCS_PER_RUN=8`) reach ~24 — fine on a
workstation, oversubscribed on a laptop. Lower `CFG_PROCS_PER_RUN` first; it costs
you per-run speed rather than concurrency.

### Input file

| Variable | Meaning |
|---|---|
| `CFG_UPF` | Path to the UPF, **relative to `emews/`** (the project root), e.g. `data/upfs/main_policy_upf.txt`. **One row per model run** — the row count sets how many runs the sweep performs. |

The launcher copies this to `experiments/<EXPID>/upf.txt`, so the experiment
directory records exactly what was run.

UPF files are not in the repo — `emews/.gitignore` excludes `data/upfs/`, because a
real sweep UPF can be very large. Generate yours before launching; `CFG_UPF` is a
local path like `CFG_RESTART_FROM` in the GA config, not something that resolves on
a fresh clone.

### Model settings — apply to every line of the UPF

| Variable | Meaning |
|---|---|
| `CFG_NUM_PERSONS` | Simulated individuals per birth cohort (`n.i`) |
| `CFG_GENDER` | `females` or `males`; selects the parameter block in `02_model_inputs.R` and the `data/*_<gender>.RData` inputs |
| `CFG_END_YEAR` | Final simulation year; `cohorts` becomes `1900:CFG_END_YEAR` |
| `CFG_POLICY_YEAR` | Year policy effects begin |
| `CFG_DYEAR` | Year discounting starts (`d.year`) |
| `CFG_DC` | Discount rate (`d.c`), e.g. `0.03` |

These are per-*sweep*, not per-line. To vary the policy year or the discount rate
you need separate experiments.

#### Year constraints

These produce corrupt output rather than an error:

- **`CFG_DYEAR <= CFG_END_YEAR`.** `03_model_functions.main()` builds `v.year_range` as
  `d.year:max(cohorts)` where `cohorts` is `1900:end_year`. Policy sweeps want
  `CFG_END_YEAR=2100`.
- **`CFG_POLICY_YEAR < CFG_END_YEAR`, and `< 2100`.** The policy block in
  `03_model_functions.main_calib()` writes `p.NC[, (policyyear+1):2100]` with a hardcoded 2100 alongside
  `p.NO.NE[, (policyyear+1):endyear]`.

### Scheduler

The variables control how the run is queued on an HPC machine.
**Not relevant for a local run.**

These are read and echoed, and exported for Swift/T, but are **ignored while
`MACHINE=""`** in `local_run_sweep.sh`. They matter only if you set `MACHINE` to
`pbs`, `slurm`, etc. for a **non-local** HPC cluster run.

| Variable | Meaning |
|---|---|
| `CFG_WALLTIME` | Job walltime, e.g. `10:00:00` |
| `CFG_QUEUE` | Scheduler queue |
| `CFG_PROJECT` | Scheduler project / allocation |

---

## 3. The UPF

> **One row = one model run.** The UPF is a list of runs, not a single
> configuration. A UPF with 200 lines launches 200 model runs and writes 200
> result files; a UPF with one line runs the model once. The number of lines *is*
> the size of your sweep — it is the only thing that controls it. Nothing in the
> cfg multiplies it.

Each line is one JSON dictionary of key value pairs, and it must be valid JSON **on a single physical
line** — `sweep.swift` calls `file_lines()` and hands each line onward whole, so a
pretty-printed object spanning several lines will be read as several broken runs.
Write the JSON compact (`jsonlite::toJSON(..., auto_unbox = TRUE)` on one
`writeLines()` call per run).

Line *i* (counting from 1) produces `results/<i>_result.RData`, so output files map
back to UPF rows by position. The UPF that produced a set of results is copied to
`experiments/<EXPID>/upf.txt`.

```json
{"policy":"main","seednew":1,"v.params":{"s.NC_18.23_9.17":1.5114702355,"...":"..."},"policy_effects":{"rr.init_1":0.37,"...":"..."}}
```

Formatted for reading:

```json
{
  "policy": "main",
  "seednew": 1,
  "v.params": {
    "s.NC_18.23_9.17": 1.511470235518387,
    "s.CF_18.23_15.25": 0.7277891718786511,
    "...": "one entry per calib == 1 parameter"
  },
  "policy_effects": {
    "rr.init_1": 0.37, "rr.init_s": 0.35,
    "rr.cess_1": 0.36, "rr.cess_s": 0.34,
    "p.CO.CE_1": 0.61, "p.CO.CE_s": 0.51,
    "p.CO.FE_1": 0.56, "p.CO.FE_s": 0.58,
    "p.NO.NE_1": 0.315, "p.NO.NE_s": 0.325,
    "s.EX": 0.1, "s.HD_2100": 1
  }
}
```

### Fields

**`policy`** — optional string, default `"sweep"`. A label only: `main()` takes it
as an argument but never reads it. It is saved into the result file and printed in
the log, so use it to identify the scenario.

**`seednew`** — optional number, default `1`. The RNG seed, read by `set.seed()`
inside `mds_microsim`. Two lines differing only in `seednew` give you replicates.

**`v.params`** — required object, one entry per parameter flagged `calib == 1` in
`m.calib_inputs` ([`../../R/02_model_inputs.R`](../../R/02_model_inputs.R)). Currently
**40 parameters for both genders**. Keys are the `m.calib_inputs` row names.

Validation happens before the model runs, in `check_v_params()`:

- A missing `calib == 1` name is a **hard error**. This matters because
  `get_value()` indexes `v.params` by name and a missing name yields `NA` silently,
  poisoning the whole run rather than failing.
- A name not in `m.calib_inputs` is a **warning**, and the entry is ignored. Usually
  a typo or a stale name from before a rename.

Parameters with `calib == 0` are pinned at their `m.calib_inputs` value and must
*not* appear here.

**`policy_effects`** — optional object. Omit it, or pass `null` or `{}`, to run the
baseline scenario with no policy applied. When present it must carry **all twelve**
fields; a missing one is a hard error naming what is absent. The twelve are the
formal arguments of `apply_policy()`, in order:

| Field | Meaning |
|---|---|
| `rr.init_1` / `rr.init_s` | Smoking initiation multiplier, policy year / subsequent years |
| `rr.cess_1` / `rr.cess_s` | Smoking cessation probability, policy year / subsequent |
| `p.CO.CE_1` / `p.CO.CE_s` | Current smoker takes up vaping |
| `p.CO.FE_1` / `p.CO.FE_s` | Current smoker switches to vaping |
| `p.NO.NE_1` / `p.NO.NE_s` | Never smoker takes up vaping |
| `s.EX` | Vaping mortality scaling (baseline 0.1) |
| `s.HD_2100` | Depression trend switch; `1` keeps the trend, `0` changes it |

`1.0` means no change for the multiplier fields. Note the code only applies an
override when the value differs from 1, so a value of exactly 1 leaves the baseline
matrix untouched.

Values are **assigned**, not scaled: `p.CF`, `p.CO.CE` and `p.CO.FE` are written
directly into the probability matrices. They are competing exits from the same
state and share one unit of probability with depression incidence, so their sum can
exceed 1. When it does, the run returns `status="invalid_probs"` rather than
results. See [`../R/test/policy_arithmetic.html`](../R/test/policy_arithmetic.html)
for which of the `06_analysis.R` scenarios are affected and why.

### Worked examples

Two complete, runnable UPF lines sit next to this document. **Each file contains
exactly one line, so each is a one-run sweep** — they are minimal illustrations of
the line format, not examples of a realistic sweep size. A real UPF has as many
rows as you have runs.

Both carry the same 40 `v.params` — row 1 of an EMEWS GA run for females,
generation 91, goodness-of-fit −977.2174 — so the only difference between them is
the policy:

| File | Lines | `policy` | `policy_effects` | What it runs |
|---|---|---|---|---|
| [`example_baseline_upf.txt`](example_baseline_upf.txt) | 1 | `"baseline"` | **absent** | Status quo. No policy applied; `l.policy_effects` is `NULL` in the result file. |
| [`example_main_policy_upf.txt`](example_main_policy_upf.txt) | 1 | `"main"` | all 12 fields | The `main` scenario from [`../../R/06_analysis.R`](../../R/06_analysis.R): the expected-case tobacco policy. |

The baseline file simply omits the `policy_effects` key — that is the whole
difference, and it is what makes `build_policy_effects()` return `NULL`:

```
{"policy":"baseline","seednew":1,"v.params":{...}}
{"policy":"main","seednew":1,"v.params":{...},"policy_effects":{"rr.init_1":0.37,...,"s.HD_2100":1}}
```

Because one row is one run, concatenating the two files is all it takes to turn two
one-run sweeps into a two-run comparison — baseline against policy, with parameters
and seed held fixed:

```bash
cd emews
cat docs/example_baseline_upf.txt docs/example_main_policy_upf.txt > data/upfs/compare.txt
# then set CFG_UPF=data/upfs/compare.txt in the cfg
```

That produces `results/1_result.RData` (baseline) and `results/2_result.RData`
(main), in UPF line order.

Two caveats before you reuse these values. The `v.params` names track
`m.calib_inputs`, so a rename or a change to the `calib` column in
`02_model_inputs.R` will invalidate them — regenerate rather than hand-edit. And
the `main` scenario's effects sum past the available probability mass, so that line
is expected to come back with `status="invalid_probs"` and `NULL` results; it is a
valid exercise of the workflow, not a valid model result.

---

## 4. Workflow flow

```
  local_run_sweep.sh        (bash, your shell)
        │  swift-t -n PROCS
        ▼
  sweep.swift               (Swift/T, PROCS MPI ranks)
        │  foreach line of upf.txt → obj()
        │  python_persist
        ▼
  launch.run_sweep()        (Python, embedded in the Swift/T worker)
        │  subprocess
        ▼
  run_mdse_microsim_sweep.sh  (bash; sources local_env.sh, leaves conda)
        │  Rscript
        ▼
  run_model_sweep.R         (R; sources 01/02/03, calls main())
        │
        ▼
  results/<i>_result.RData
```

### 4.1 `swift/local_run_sweep.sh`

Takes `<EXPID>` and a cfg path. In order:

1. Sets `EMEWS_PROJECT_ROOT` to `emews/` (resolved from its own location) and
   sources [`../etc/emews_utils.sh`](../etc/emews_utils.sh) for `check_directory_exists`
   and `log_script`.
2. Sets `TURBINE_OUTPUT` to `$EMEWS_PROJECT_ROOT/experiments/$EXPID`, then
   `check_directory_exists` prompts if it already exists.
3. Sources the cfg, echoes a summary, and exports `PROCS`, `QUEUE`, `PROJECT`,
   `WALLTIME`, `PPN`, `TURBINE_JOBNAME`, `TURBINE_MPI_THREAD`.
4. Sets `PYTHONPATH` to `emews/python:emews/ext/EQ-Py` so Swift/T's embedded Python
   can `import launch`.
5. Sets `SITE=local`, which selects `scripts/local_env.sh` further down the chain.
6. `MACHINE=""` → unscheduled. Because there is no scheduler to capture output, the
   script does `exec &> "$TURBINE_OUTPUT/output.txt"` — **everything after this point
   goes to that file**.
7. Creates `tmp/` and `results/`, copies the cfg to `cfg.cfg`, the UPF to `upf.txt`,
   and `scripts/run_mdse_microsim_sweep.sh` into the experiment directory. The
   Python step later invokes **that copy**, not the original, so the experiment
   records the wrapper it actually used.
8. Builds `CMD_LINE_ARGS`:
   `-f=<upf> -num_p -gender -end_year -r_file=run_model_sweep.R -policyyear -dyear -dc`.
   Note `-r_file` is how the same downstream plumbing serves both the GA
   (`run_model.R`) and the sweep.
9. `log_script` writes the resolved environment and the script itself to
   `local_run_sweep.sh.log`.
10. Invokes `swift-t`, with `-I`/`-r` pointing at `ext/emews` (so `import emews;`
    resolves) and `-e` forwarding `TURBINE_MPI_THREAD`, `TURBINE_OUTPUT`,
    `EMEWS_PROJECT_ROOT` and `PROCS_PER_RUN`.

### 4.2 `swift/sweep.swift`

Reads the command-line arguments, then:

```swift
main() {
    string upf_lines[] = file_lines(upf);
    foreach s, i in upf_lines {
        obj(s, i);
    }
}
```

`foreach` is parallel — Swift/T distributes iterations across the worker ranks, so
the number of concurrent model runs is bounded by available ranks, not by the UPF
length. `i` is the line's **1-based** index, and becomes the result filename.

`obj()` builds a short Python snippet from `run_model_template`, substituting the
parameter line, the model settings and `result_f`
(`$TURBINE_OUTPUT/results/<i>_result.RData`), then runs it with `python_persist`.
The snippet's only job is to call `launch.run_sweep(...)`.

`import emews;` pulls in [`../ext/emews/emews.swift`](../ext/emews/emews.swift). The
sweep does not currently call its `parse_json_list()`, but the import must resolve
for the script to compile.

### 4.3 `python/launch.py` → `run_sweep()`

Runs inside the Swift/T worker's embedded Python interpreter.

1. Reads `TURBINE_OUTPUT` and `EMEWS_PROJECT_ROOT` from the environment; derives
   `mdse_code_dir` as `EMEWS_PROJECT_ROOT/../`, i.e. the MDSE repository root.
2. Locates the wrapper copy at `$TURBINE_OUTPUT/run_mdse_microsim_sweep.sh` and
   `chmod`s it executable — `cp` does not always preserve the bit.
3. Copies the environment and sets `MDSE_NUM_CORES` from `PROCS_PER_RUN`.
4. Runs the wrapper as a subprocess with ten arguments, capturing output.
5. Writes `tmp/<run_id>_out.txt` and `tmp/<run_id>_err.txt` — **always**, on success
   as well as failure, with the runtime in seconds appended to the stdout file.
   (The GA's `run()` writes these only on failure.)

### 4.4 `scripts/run_mdse_microsim_sweep.sh`

1. Sources `$EMEWS_PROJECT_ROOT/scripts/${SITE}_env.sh` — with `SITE=local` that is
   [`../scripts/local_env.sh`](../scripts/local_env.sh).
2. Sets `OMP_NUM_THREADS=1` and `MKL_THREADS=1`, so the R workers do not each spawn
   their own BLAS thread pool on top of the fork cluster.
3. Reads ten positional arguments: gender, end year, n.i, result file, the JSON
   parameter line, the MDSE repo root, the R filename, policy year, d.year, d.c.
4. `cd $TURBINE_OUTPUT`, then `Rscript $EMEWS_PROJECT_ROOT/R/$R_FILE` with the nine
   model arguments.

#### R comes from outside conda

`local_env.sh` is four meaningful lines:

```bash
eval "$(conda shell.bash hook)"
conda deactivate
conda deactivate
```

The double deactivate is deliberate and commented as such. One `deactivate` drops
from `emews-py3.11` to `base`; the second leaves conda entirely. The effect:

| | `Rscript` resolves to |
|---|---|
| Inside `emews-py3.11` | `.../envs/emews-py3.11/bin/Rscript` |
| After double deactivate | `/usr/bin/Rscript` |

So **the model runs under the system R, not the conda R**. The MDSE package set —
`darthtools`, `dampack`, `matrixStats`, `foreach`, `doParallel` and the rest of
`01_environment.R` — must be installed for the *system* R. Installing them into the
conda environment has no effect on the sweep.

Adapting to another machine means adding `scripts/<site>_env.sh`, a matching
`swift/<site>_run_sweep.sh`, and `data/cfgs/<site>_sweep.cfg`, then setting `SITE`.

### 4.5 `R/run_model_sweep.R`

The per-run entry point.

1. Reads the nine CLI arguments. Builds `args <- c(gender, n.i, end_year)` — the
   positional vector `02_model_inputs.R` expects, note the reordering — and
   normalises `mainDir` to end in `/`, because `02_model_inputs.R` builds data paths
   as `paste0(mainDir, "data/...")` with no separator of its own.
2. Sources `01_environment.R`, `02_model_inputs.R`, `03_model_functions.R` in that
   order. The order is required; see the sourcing contract in the repository
   [`CLAUDE.md`](../../CLAUDE.md).
3. Calls `run()`, which:
   - Opens a `FORK` cluster of `MDSE_NUM_CORES` workers on a random free port,
     retrying up to ten times — concurrent evaluations on one node race for ports.
   - Parses the JSON line.
   - Sets the globals `main()` and `main_calib()` read but that `01`/`02`/`03` never
     define: `policyyear`, `d.year`, `d.c` (from the CLI), `seednew` (from the JSON)
     and `v.affected_ages`.
   - Validates `v.params` via `check_v_params()`.
   - Builds `l.policy_effects` by delegating to `apply_policy()`, so the field names
     and numeric coercion live in one place, or `NULL` for baseline.
   - Calls `main(v.params, l.policy_effects, policy)`.
   - Derives `status` and saves.

---

## 5. Output

```
emews/experiments/<EXPID>/
├── output.txt                   all launcher + Swift/T output (unscheduled runs)
├── cfg.cfg                      copy of the cfg used
├── upf.txt                      copy of the UPF used
├── local_run_sweep.sh.log       resolved environment + the launcher script
├── run_mdse_microsim_sweep.sh   copy of the wrapper actually invoked
├── tmp/
│   ├── <i>_out.txt              stdout of run i, with runtime appended
│   └── <i>_err.txt              stderr of run i
└── results/
    └── <i>_result.RData         one per UPF line, i is the line index (starting at 1)
```

`experiments/` is gitignored.

### What is in a result file

Every run writes a loadable file with the same object names, whether or not the
parameters were valid:

| Object | Contents |
|---|---|
| `status` | `"ok"`, or `"invalid_probs"` |
| `l.results` | Total population. `NULL` when `status` is `"invalid_probs"` |
| `l.results_D` | Currently depressed subpopulation |
| `l.results_ND` | Not currently depressed |
| `policy` | The label from the UPF line |
| `v.params` | The parameters used |
| `l.policy_effects` | The twelve effects, or `NULL` for baseline |

The three result lists use the names `06_analysis.R` saves, so
`reformat_model_outputs()` and the `07_*` figure scripts load them unchanged. Each
holds `l.model_prevs`, `m.cuw` (costs / QALYs / productivity / consumer expenditure
/ societal costs / life-years), `v.lifeyears`, the smoking-attributable death
series, `init`, `cess` and `v.deathrate`.

**Check `status` before touching `l.results`.** An out-of-range parameter set or
policy is an expected outcome of sweeping, not a crash — `main()` returns empty
results and the run exits 0.

### Collecting results

```r
files <- list.files("emews/experiments/<EXPID>/results", full.names = TRUE)
rows <- lapply(files, function(f) {
  e <- new.env(); load(f, envir = e)
  data.frame(file = basename(f),
             policy = get("policy", e),
             status = get("status", e),
             stringsAsFactors = FALSE)
})
summary <- do.call(rbind, rows)
table(summary$status)

# then load the ones that produced output
ok <- summary$file[summary$status == "ok"]
```

---

## 6. Troubleshooting

**Nothing on the terminal after the prompt.** Expected — `exec &>` redirects to
`experiments/<EXPID>/output.txt`. Tail it.

**`there is no package called 'darthtools'`** (or `dampack`, `matrixStats`, …).
The packages are installed for the conda R, not the system R. See
[R comes from outside conda](#r-comes-from-outside-conda). `01_environment.R` will
try to install them, which also pulls `devtools` and can fail on missing system
libraries (fontconfig, harfbuzz, libgit2).

**`v.params is missing N calibrated parameter(s): …`** The UPF predates a change to
the `calib` column or a parameter rename in `02_model_inputs.R`. Regenerate it
against the current names.

**`v.params has N name(s) not in m.calib_inputs (ignored)`** Warning, not fatal. The
named parameters are being silently dropped and those bands will use their pinned
`m.calib_inputs` values.

**Every run returns `status="invalid_probs"`.** The policy effects sum past the
available probability mass. Check against
[`../R/test/policy_arithmetic.html`](../R/test/policy_arithmetic.html); it is not a
workflow fault.

**Machine bogs down.** `(CFG_PROCS - 1) × CFG_PROCS_PER_RUN` processes. Lower
`CFG_PROCS_PER_RUN`.

**Permission denied on `run_mdse_microsim_sweep.sh`.** `run_sweep()` chmods the copy
before invoking it; if this appears, the copy step in the launcher did not run —
check `output.txt` for an earlier failure.
