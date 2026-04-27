module load gcc
module load openblas
module load R

export R_HOME="/software/software/custom-built/R/4.5.1/gcc-14.2.0/lib64/R"
export R_LIBS_USER=/lcrc/project/EMEWS/improv/rlibs/4.5
export LD_LIBRARY_PATH=/lcrc/project/EMEWS/improv/sfw/libdeflate-1.24/lib64:$LD_LIBRARY_PATH