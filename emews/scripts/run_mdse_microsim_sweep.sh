#!/bin/bash

set -eu

# Check for an optional timeout threshold in seconds. If the duration of the
# model run as executed below, takes longer that this threshhold
# then the run will be aborted. Note that the "timeout" command
# must be supported by executing OS.

# The timeout argument is optional. By default the "run_model" swift
# app fuction sends 3 arguments, and no timeout value is set. If there
# is a 4th (the TIMEOUT_ARG_INDEX) argument, we use that as the timeout value.

# !!! IF YOU CHANGE THE NUMBER OF ARGUMENTS PASSED TO THIS SCRIPT, YOU MUST
# CHANGE THE TIMEOUT_ARG_INDEX !!!
# TIMEOUT=""
# TIMEOUT_ARG_INDEX=6
# if [[ $# ==  $TIMEOUT_ARG_INDEX ]]
# then
# 	TIMEOUT=${!TIMEOUT_ARG_INDEX}
# fi

# TIMEOUT_CMD=""
# if [ -n "$TIMEOUT" ]; then
#   TIMEOUT_CMD="timeout $TIMEOUT"
# fi

source $EMEWS_PROJECT_ROOT/scripts/${SITE}_env.sh

export OMP_NUM_THREADS=1
export MKL_THREADS=1

echo $LD_LIBRARY_PATH


# whichgender ("females, males"), end_year, num_individuals, result_file, parameters, r_main_dir

GENDER=$1
END_YEAR=$2
NUM_INDIV=$3
RESULT_FILE=$4
PARAMS=$5
MDSE_CODE_DIR=$6
R_FILE=$7
POLICYYEAR=$8
DYEAR=$9
DC=${10}

cd $TURBINE_OUTPUT

# readonly PORT=$(python3 -c 'import socket; s=socket.socket(); s.bind(("", 0)); print(s.getsockname()[1]); s.close()')

arg_array=( "$EMEWS_PROJECT_ROOT/R/$R_FILE" 
            "$GENDER"
            "$END_YEAR"
            "$NUM_INDIV"
            "$RESULT_FILE"
            "$PARAMS"
            "$MDSE_CODE_DIR"
            "$POLICYYEAR"
            "$DYEAR"
            "$DC" )


echo $( which Rscript )
# echo $( ls /lib64/libd* )
Rscript "${arg_array[@]}"

#$TIMEOUT_CMD $COMMAND
# $? is the exit status of the most recently executed command (i.e the
# line above)
RES=$?
if [ "$RES" -ne 0 ]; then
	if [ "$RES" == 124 ]; then
    echo "---> Timeout error in $COMMAND"
  else
	  echo "---> Error in $COMMAND"
  fi
fi
