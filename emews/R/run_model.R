library(jsonlite)

run <- function(mainDir, result_file, input_string) {
    n.cores <- Sys.getenv("MDSE_NUM_CORES")
    
    i <- 0
    cl <- NULL
    while (i < 10) {
        port <- as.integer(system("/gpfs/fs1/soft/improv/software/spack-built/linux-rhel8-zen3/gcc-13.2.0/python-3.11.6-v7avskv/bin/python3 -c 'import socket; s=socket.socket(); s.bind((\"\", 0)); print(s.getsockname()[1]); s.close()'", intern = TRUE))
        cat(paste0("n.cores: ", n.cores, " port: ", port, "\n"))
        tryCatch({
            cl <- makeCluster(as.numeric(n.cores), port=port, type = "FORK") # , outfile = "./log_file.txt")
            registerDoParallel(cl)
            i <- 11
        }, error = function(cond) {
            i <- i + 1
            if (i == 10) {
                stop(cond)
            }
        })
    }

    v.target_names <<- names(l.calib_targets) # number of calibration targets
    n.target <<- length(v.target_names)
    input_params <- unlist(fromJSON(input_string))
    # print(input_params)
    # cat("\n")
    # TODO: fix -- can't find where this is set in the R code
    seednew <<- 42
    result = f_gof(input_params)
    # cat(paste0("GOF: ", result, "\n"))
    stopCluster(cl)
    writeLines(paste0(result), result_file)
}

library(yaml)
# whichgender ("females, males"), end_year, num_individuals, result_file, parameters, r_main_dir
cli_args <- commandArgs(trailingOnly = TRUE)
# whichgender, n.i, endyear
args = c(cli_args[1], cli_args[3], cli_args[2])
mainDir <- cli_args[6]

s.HD_2100 <- 1
source(paste0(mainDir,"/R/01_environment.R"), echo=FALSE)
source(paste0(mainDir,"/R/02_model_inputs.R"), echo=FALSE)
source(paste0(mainDir,"/R/03_model_functions.R"), echo=FALSE) # microsimulation model and probability functions

run(cli_args[6], cli_args[4], cli_args[5])
