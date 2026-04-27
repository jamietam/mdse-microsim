#! /usr/bin/env bash

set -eu

if [ "$#" -ne 2 ]; then
  script_name=$(basename $0)
  echo "Usage: ${script_name} EXPERIMENT_ID cfg_file"
  exit 1
fi


# uncomment to turn on swift/t logging. Can also set TURBINE_LOG,
# TURBINE_DEBUG, and ADLB_DEBUG to 0 to turn off logging
export TURBINE_LOG=0 TURBINE_DEBUG=0 ADLB_DEBUG=0
#export TURBINE_STDOUT=out-%%r.txt
export TURBINE_STDOUT=
export EMEWS_PROJECT_ROOT=$( cd $( dirname $0 )/.. ; /bin/pwd )
# source some utility functions used by EMEWS in this script
source "${EMEWS_PROJECT_ROOT}/etc/emews_utils.sh"

export EXPID=$1
export TURBINE_OUTPUT=$EMEWS_PROJECT_ROOT/experiments/$EXPID
check_directory_exists

CFG_FILE=$2
source $CFG_FILE


echo "--------------------------"
echo "WALLTIME:            $CFG_WALLTIME"
echo "PROCS:               $CFG_PROCS"
echo "PROJECT:             $CFG_PROJECT"
echo "PROCS_PER_RUN        $CFG_PROCS_PER_RUN"
echo "GENDER:              $CFG_GENDER"
echo "N.I:                 $CFG_NUM_PERSONS"
echo "RESTART FROM:        $CFG_RESTART_FROM"
# echo "MKL_THREADS:         $CFG_MKL_THREADS"
echo "--------------------------"

# TODO edit the number of processes as required.
export PROCS=$CFG_PROCS

# TODO edit QUEUE, WALLTIME, PPN, AND TURNBINE_JOBNAME
# as required. Note that QUEUE, WALLTIME, PPN, AND TURNBINE_JOBNAME will
# be ignored if the MACHINE variable (see below) is not set.
#export QUEUE=R.CVD_Research_3
export QUEUE=$CFG_QUEUE
#export QUEUE=debug-flat-quad
export WALLTIME=$CFG_WALLTIME
export PPN=$CFG_PPN
export TURBINE_JOBNAME="${EXPID}_job"
export PROJECT=$CFG_PROJECT
# export CFG_MKL_THREADS

# Resident task workers and ranks
export TURBINE_RESIDENT_WORK_WORKERS=1
export RESIDENT_WORK_RANKS=$(( PROCS - 2 ))

EQPY=$EMEWS_PROJECT_ROOT/ext/EQ-Py
export PYTHONPATH=$EMEWS_PROJECT_ROOT/python:$EQPY

# TODO edit command line arguments, e.g. -nv etc., as appropriate
# for your EQ/Py based run. Note that $* will pass any of this
# script's command line arguments to swift-t
CMD_LINE_ARGS="$*"

# set machine to your schedule type (e.g. pbs, slurm, cobalt etc.),
# or empty for an immediate non-queued unscheduled run
MACHINE="pbs"
export TURBINE_IMPROV=1

export SITE=improv

if [ -n "$MACHINE" ]; then
  MACHINE="-m $MACHINE"
fi

export ADLB_SERVERS=1 

mkdir -p $TURBINE_OUTPUT/tmp
mkdir -p $TURBINE_OUTPUT/results
cp $CFG_FILE $TURBINE_OUTPUT/cfg.cfg
cp $EMEWS_PROJECT_ROOT/scripts/run_mdse_microsim.sh $TURBINE_OUTPUT/

if [ -z "$CFG_RESTART_FROM" ]; then
        RESTART_FROM="null"
else
  RESTART_FROM="$TURBINE_OUTPUT/restart.pkl"
  cp $CFG_RESTART_FROM $RESTART_FROM
fi

DEAP_CFG=$TURBINE_OUTPUT/ga_cfg.yaml
cat > $DEAP_CFG << EOF
init_pop_path: ${TURBINE_OUTPUT}/calib_inputs.csv
ngen: ${CFG_NGEN}
mutpb: ${CFG_MUTPB}
cxpb: ${CFG_CXPB}
popsize: ${CFG_POP_SIZE}
eta: ${CFG_ETA}
indpb: ${CFG_INDPB}
hof: ${CFG_HOF}
lambda_: ${CFG_LAMBDA}
mu: ${CFG_MU}
restart_from: ${RESTART_FROM}
EOF


# RLIB="/lcrc/project/EMEWS/improv/sfw/gcc-13.2.0/R-4.3.2/lib64/R/lib"
# READLINE_LIB="/lcrc/project/EMEWS/improv/sfw/gcc-13.2.0/readline-8.2/lib"
# GETTEXT_LIB="/gpfs/fs1/soft/improv/software/spack-built/linux-rhel8-zen3/gcc-13.2.0/gettext-0.22.3-ycr7uf7/lib"
# MKL1="/gpfs/fs1/soft/improv/software/spack-built/linux-rhel8-zen3/gcc-13.2.0/intel-mkl-2020.4.304-zhhmifa/compilers_and_libraries_2020.4.304/linux/mkl/lib/intel64_lin"
# MKL2="/gpfs/fs1/soft/improv/software/spack-built/linux-rhel8-zen3/gcc-13.2.0/intel-mkl-2020.4.304-zhhmifa/compilers_and_libraries_2020.4.304/linux/compiler/lib/intel64_lin"
# MKL3="/gpfs/fs1/soft/improv/software/spack-built/linux-rhel8-zen3/gcc-13.2.0/intel-mkl-2020.4.304-zhhmifa/compilers_and_libraries_2020.4.304/linux/tbb/lib/intel64_lin/gcc4.1"
# OPENMPI="/gpfs/fs1/soft/improv/software/spack-built/linux-rhel8-zen3/gcc-13.2.0/openmpi-4.1.6-knzkvb2/lib"
OPENMPI="/gpfs/fs1/soft/improv/software/spack-built/linux-rhel8-zen3/gcc-13.2.0/openmpi-5.0.2-7bjpmj4/lib"
# LLP=$RLIB:$OPENMPI:$READLINE_LIB:$GETTEXT_LIB:$MKL1:$MKL2:$MKL3

# See https://www.mail-archive.com/devel@lists.open-mpi.org/msg21434.html
export LD_PRELOAD="${OPENMPI}/libmpi.so"

export TURBINE_LAUNCH_OPTIONS="--bind-to none"
export TURBINE_DIRECTIVE="#PBS -l place=free"
# export TURBINE_LAUNCH_OPTIONS="-n $PROCS -N $PPN --bind-to none"
# export TURBINE_IMPROV=1

export TURBINE_PRELAUNCH="Rscript $EMEWS_PROJECT_ROOT/R/create_init_pop.R $TURBINE_OUTPUT ${EMEWS_PROJECT_ROOT}/../ $CFG_GENDER $CFG_END_YEAR $CFG_NUM_PERSONS"


CMD_LINE_ARGS="-num_p=${CFG_NUM_PERSONS} -gender=${CFG_GENDER} -end_year=${CFG_END_YEAR} -ga_cfg=$DEAP_CFG"

# Add any script variables that you want to log as
# part of the experiment meta data to the USER_VARS array,
# for example, USER_VARS=("VAR_1" "VAR_2")
USER_VARS=("MODEL_DIR" "INPUT_FILE" "STOP_AT" "MODEL_PROPS")
# log variables and script to to TURBINE_OUTPUT directory
log_script

# echo's anything following this standard out
set -x

swift-t -n $PROCS $MACHINE -p \
    -r $EQPY -I $EQPY \
    -e EMEWS_PROJECT_ROOT \
    -e ADLB_SERVERS \
    -e TURBINE_OUTPUT \
    -e TURBINE_LOG \
    -e TURBINE_DEBUG \
    -e ADLB_DEBUG \
    -e PYTHONPATH \
    -e LD_PRELOAD \
    -e PROCS_PER_RUN=$CFG_PROCS_PER_RUN \
  $EMEWS_PROJECT_ROOT/swift/ga.swift "$CMD_LINE_ARGS"

chmod g+rw $TURBINE_OUTPUT/*.tic
