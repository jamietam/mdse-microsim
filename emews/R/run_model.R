library(jsonlite)

run <- function(mainDir, result_file, input_string, port) {

    

    n.cores <- Sys.getenv("MDSE_NUM_CORES")
    cat(paste0("n.cores: ", n.cores, " port: ", port, "\n"))
    cl <- makeCluster(as.numeric(n.cores), port=as.numeric(port), type = "FORK") # , outfile = "./log_file.txt")
    registerDoParallel(cl)

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

run(cli_args[6], cli_args[4], cli_args[5], cli_args[7])
