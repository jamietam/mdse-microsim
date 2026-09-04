# Per-run entry point for the EMEWS parameter sweep.
#
# Runs one policy scenario through main() in ../R/03_model_functions.R and saves
# the full result set. This is the batch equivalent of the run_scenarios == 1
# branch of ../R/06_analysis.R. (The GA workflow's run_model.R instead calls
# f_gof() and returns a single scalar.)
#
# Usage:
#   Rscript run_model_sweep.R <gender> <end_year> <n.i> <result_file> <json> \
#                              <mdse_main_dir> <policyyear> <d.year> <d.c>
#
# <json> is a single dictionary string, e.g.
#   {"policy": "main",
#    "seednew": 1,
#    "v.params": {"s.NC_9.17": 1.677217, ...},
#    "policy_effects": {"rr.init_1": 0.37, ..., "s.HD_2100": 1}}
#
# Omit "policy_effects" (or pass null / {}) to run the baseline scenario.
#
# <result_file> always gets written, and always contains the same objects:
#   status            "ok", or "invalid_probs" when this parameter set / policy
#                     drives a transition probability out of range
#   l.results         total population    ) NULL when status is "invalid_probs"
#   l.results_D       currently depressed )
#   l.results_ND      not currently depressed
#   policy, v.params, l.policy_effects    the inputs, so the file is self-describing
#
# YEAR CONSTRAINTS -- these produce corrupt output rather than an error:
#   * d.year <= end_year.  main() builds v.year_range as d.year:max(cohorts),
#     and cohorts is 1900:end_year.  The GA config's end_year of 2023 with a
#     2027 policy year yields a REVERSED range.  Policy sweeps want 2100.
#   * policyyear < end_year, and policyyear < 2100.  The policy block in
#     main_calib writes p.NC[, (policyyear+1):2100] with a hardcoded 2100
#     alongside p.NO.NE[, (policyyear+1):endyear].

library(jsonlite)

# The 12 fields apply_policy() expects, in its formal argument order.
POLICY_EFFECT_NAMES <- c("rr.init_1", "rr.init_s",
                         "rr.cess_1", "rr.cess_s",
                         "p.CO.CE_1", "p.CO.CE_s",
                         "p.CO.FE_1", "p.CO.FE_s",
                         "p.NO.NE_1", "p.NO.NE_s",
                         "s.EX", "s.HD_2100")

# Read a scalar from the parsed JSON, falling back to a default when absent.
spec_value <- function(spec, key, default) {
    if (is.null(spec[[key]])) default else as.numeric(spec[[key]])
}

# Retry on a fresh random port each attempt -- concurrent evaluations on the
# same node race for ports.  Note the retry counter is advanced by the loop
# rather than inside the error handler, which cannot reach it.
start_cluster <- function() {
    n.cores <- Sys.getenv("MDSE_NUM_CORES")

    for (attempt in 1:10) {
        port <- as.integer(system("python3 -c 'import socket; s=socket.socket(); s.bind((\"\", 0)); print(s.getsockname()[1]); s.close()'", intern = TRUE))
        cat(paste0("n.cores: ", n.cores, " port: ", port, "\n"))
        cl <- tryCatch({
            cluster <- makeCluster(as.numeric(n.cores), port=port, type = "FORK") # , outfile = "./log_file.txt")
            registerDoParallel(cluster)
            cluster
        }, error = function(cond) {
            if (attempt == 10) {
                stop(cond)
            }
            NULL
        })
        if (!is.null(cl)) {
            return(cl)
        }
    }
}

# v.params must carry a value for every parameter m.calib_inputs flags with
# calib == 1.  get_value() indexes v.params by name, so a missing name yields
# NA rather than an error and silently poisons the whole run.
check_v_params <- function(v.params) {
    if (is.null(names(v.params))) {
        stop("v.params has no names; expected a JSON object keyed by parameter name")
    }
    required <- rownames(m.calib_inputs)[m.calib_inputs[, "calib"] == 1]
    missing <- setdiff(required, names(v.params))
    if (length(missing) > 0) {
        stop("v.params is missing ", length(missing), " calibrated parameter(s): ",
             paste(missing, collapse = ", "))
    }
    unknown <- setdiff(names(v.params), rownames(m.calib_inputs))
    if (length(unknown) > 0) {
        warning("v.params has ", length(unknown), " name(s) not in m.calib_inputs (ignored): ",
                paste(unknown, collapse = ", "))
    }
}

# Build the policy effects list, or NULL for the baseline scenario.  Delegates
# to apply_policy() so the names and as.numeric() coercion stay in one place.
build_policy_effects <- function(pe) {
    if (is.null(pe) || length(pe) == 0) {
        return(NULL)
    }
    missing <- setdiff(POLICY_EFFECT_NAMES, names(pe))
    if (length(missing) > 0) {
        stop("policy_effects is missing ", length(missing), " field(s): ",
             paste(missing, collapse = ", "))
    }
    do.call(apply_policy,
            c(lapply(pe[POLICY_EFFECT_NAMES], as.numeric),
              list(policyyear = policyyear, v.affected_ages = v.affected_ages)))
}

run <- function(mainDir, result_file, input_string, policyyear, d.year, d.c) {
    cl <- start_cluster()
    on.exit(stopCluster(cl), add = TRUE)

    spec <- fromJSON(input_string)

    # Globals that main() / main_calib() read from the global env but that
    # 01_environment.R, 02_model_inputs.R and 03_model_functions.R never set.
    # s.HD_2100 is deliberately NOT set here: the global is never read, only
    # the element of the same name inside l.policy_effects.
    policyyear <<- policyyear # spec_value(spec, "policyyear", 2027)
    d.year <<- d.year # spec_value(spec, "d.year", policyyear)
    d.c <<- d.c # spec_value(spec, "d.c", 0.03)
    seednew <<- spec_value(spec, "seednew", 1)
    v.affected_ages <<- 0:99

    v.params <- unlist(spec$v.params)
    check_v_params(v.params)

    l.policy_effects <- build_policy_effects(spec$policy_effects)
    policy <- if (is.null(spec$policy)) "sweep" else as.character(spec$policy)

    cat(paste0("policy: ", policy, " policyyear: ", policyyear,
               " d.year: ", d.year, " d.c: ", d.c, " seednew: ", seednew, "\n"))

    out <- main(v.params, l.policy_effects, policy)

    # Saved under the names 06_analysis.R uses, so reformat_model_outputs() and
    # the 07_* figure scripts can load these files unchanged.  main() returns
    # the third element as l.results_notD.
    l.results <- out$l.results
    l.results_D <- out$l.results_D
    l.results_ND <- out$l.results_notD

    # A sweep explores combinations, so some will drive a transition probability
    # out of range.  main() reports that by returning empty results rather than
    # failing, so every instance still writes a loadable file -- check status
    # before touching l.results.
    status <- if (is.null(l.results)) "invalid_probs" else "ok"
    if (status == "invalid_probs") {
        cat("\n*** INVALID: out-of-range transition probabilities for this",
            "parameter set / policy.\n*** Writing", basename(result_file),
            "with status=\"invalid_probs\" and NULL results.\n")
    }

    save(status, l.results, l.results_D, l.results_ND,
         policy, v.params, l.policy_effects, file = result_file)
}

# whichgender ("females", "males"), end_year, num_individuals, result_file, parameters, r_main_dir
cli_args <- commandArgs(trailingOnly = TRUE)
# 02_model_inputs.R reads args as c(whichgender, n.i, endyear)
args = c(cli_args[1], cli_args[3], cli_args[2])
# Must end in a separator: 02_model_inputs.R builds data paths as
# paste0(mainDir, "data/...") without one of its own.
mainDir <- sub("/*$", "/", cli_args[6])

source(paste0(mainDir,"R/01_environment.R"), echo=FALSE)
source(paste0(mainDir,"R/02_model_inputs.R"), echo=FALSE)
source(paste0(mainDir,"R/03_model_functions.R"), echo=FALSE) # microsimulation model and probability functions

# mainDir, result_file, input_string, policyyear, d.year, d.c
run(mainDir, cli_args[4], cli_args[5], as.numeric(cli_args[7]), as.numeric(cli_args[8]),
    as.numeric(cli_args[9]))
