# GA #

cfg in data/cfgs/X_ga.cfg
swift/X_run_ga.sh:
    * TURBINE_PRELAUCH runs R/create_init_pop.R 
    * create_init_pop.R runs ../R/02_model_inputs.R to write parameter ub / lb

ga.py uses that file to create the initial population.
