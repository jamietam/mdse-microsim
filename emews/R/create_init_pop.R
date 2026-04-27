library(data.table)

run <- function(exp_dir, mainDir, params) {
    source(paste0(mainDir,"R/02_model_inputs.R"), echo=FALSE)
    # print(class(v.params))
    fwrite(cbind(m.calib_inputs[m.calib_inputs[,"calib"]==1,], name=names(v.params)), paste0(exp_dir, "/calib_inputs.csv"))

}

library(yaml)
cli_args <- commandArgs(trailingOnly = TRUE)
exp_dir <- cli_args[1]
mainDir <- cli_args[2]
which_gender <- cli_args[3]
end_year <- cli_args[4]
num_individuals <- cli_args[5]
args = c(which_gender, end_year, num_individuals)
run(exp_dir, mainDir, params)
