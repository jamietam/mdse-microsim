# How to run the Major Depression and Smoking Microsimulation Model

## R Environment:
**01_environment.R**
1. Ensure that all packages listed are installed correctly under R version 4.4.0.
2. In line 17, determine which gender ('males' vs 'females'), the number of people simulated per birth cohort (1000, 10000, etc.) and the final year of simulation (2016, 2050, 2100, etc.), and how many calibration initial parameter sets (40, 100, etc.) you want to run results for. The number of calibration initial parameter sets are irrelevant unless running '04_calibration.R'.

## Calibration: 
**04_calibration.R**
1. Set the main working directory as 'mainDir'.
2. If running the calibration on a personal computer or using the High Performance Computing (HPC) Open OnDemand interface, set hpc=0. If running the analysis on the HPC clusters, set hpc=1.
3. Determine which parameters you want to calibration for by editing lines 27-95 in '02_model_inputs.R'. This is done by changing the fourth value in each parameter vector to either 0 (do not calibrate) or 1 (do calibrate). The second and third values in the parameter vector specify the upper and lower bounds for searching the parameter space to identify the best fitting parameter value. 
4. The script will perform calibration and then use the best fit (lowest goodness-of-fit (GOF) value) parameter set to run the model.
5. The model results using the best fitting parameter set will be produced for comparison with National Survey on Drug Use and Health (NSDUH) data by sourcing '05_validation.R'. Results will be stored in the 'outputs' directory.

## Policy simulation: 
**06_analysis.R**
1. Set the main working directory as 'mainDir'.
2. Set the 'policyyear' to apply changes to smoking initiation and cessation probabilities beginning in that year.
3. Name the different policy scenarios in the 'scenarios' vector.
4. Set the policy's effects on initiation and cessation (1.0 = no change, 1.2 = 20% increase, 0.8 = 20% decrease) and the age group affected by the policy (0:99 = all ages are affected, 18:25 = young adults ages 18-25 are affected).
5. Run the script to simulate each policy scenario. Results will be stored in the 'outputs' directory.
