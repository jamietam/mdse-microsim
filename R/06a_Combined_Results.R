
rm(list = ls()) 
#this doesn't work right now?
# Sys.setenv(RGL_USE_NULL=TRUE) 
# Sys.setenv('R_MAX_VSIZE'=64000000000)
# Set working directory
mainDir = "/Users/srs475/Library/CloudStorage/Dropbox-UniversityofMichigan/Sarah Skolnick/GitHub/mdse-microsim/"
# mainDir = "/Users/jt936/Dropbox/GitHub/mdse-microsim/"
# mainDir = "/gpfs/gibbs/project/tam_jamie/jt936/mds-microsim/" 
setwd(mainDir)
#specify the output files you want to combine:
gender=2 #1 for male and 2 for female
policyyear=2030
if (policyyear==2030){policylabel="2030"
if (gender==1){genderlabel="male"
  file_names <- c(
  #2030 MALES LIST:
  "output/1_2030rnc_1males_depression_1000_01.31.26_12.17PM.RData",
  "output/2_2030rnc_2males_depression_1000_01.31.26_12.35PM.RData",
  "output/3_2030rnc_3males_depression_1000_01.31.26_12.55PM.RData",
  "output/4_2030rnc_4males_depression_1000_01.31.26_02.15PM.RData",
  "output/5_2030rnc_5males_depression_1000_01.31.26_02.33PM.RData",
  "output/6_2030rnc_6males_depression_1000_01.31.26_02.52PM.RData",
  "output/7_2030rnc_7males_depression_1000_01.31.26_03.10PM.RData",
  "output/8_2030rnc_8males_depression_1000_01.31.26_03.29PM.RData",
  "output/9_2030rnc_9males_depression_1000_01.31.26_03.47PM.RData",
  "output/10_2030rnc_10males_depression_1000_01.31.26_05.13PM.RData",
  "output/11_2030rnc_11males_depression_1000_01.31.26_05.31PM.RData",
  "output/12_2030rnc_12males_depression_1000_01.31.26_06.05PM.RData",
  "output/13_2030rnc_13males_depression_1000_01.31.26_06.23PM.RData",
  "output/14_2030rnc_14males_depression_1000_01.31.26_06.42PM.RData",
  "output/15_2030rnc_15males_depression_1000_01.31.26_07.52PM.RData",
  "output/16_2030rnc_16males_depression_1000_01.31.26_08.50PM.RData",
  "output/17_2030rnc_17males_depression_1000_01.31.26_10.13PM.RData",
  "output/18_2030rnc_18males_depression_1000_01.31.26_10.32PM.RData",
  "output/19_2030rnc_19males_depression_1000_01.31.26_10.50PM.RData",
  "output/20_2030rnc_20males_depression_1000_01.31.26_11.50PM.RData"
  )}
  else{genderlabel="female"
  file_names <- c(
  #2030 FEMALES LIST:
  "output/1_2030rnc_1females_depression_1000_01.31.26_11.54AM.RData",
  "output/2_2030rnc_2females_depression_1000_01.31.26_12.26PM.RData",
  "output/3_2030rnc_3females_depression_1000_01.31.26_12.45PM.RData",
  "output/4_2030rnc_4females_depression_1000_01.31.26_01.04PM.RData",
  "output/5_2030rnc_5females_depression_1000_01.31.26_02.24PM.RData",
  "output/6_2030rnc_6females_depression_1000_01.31.26_02.42PM.RData",
  "output/7_2030rnc_7females_depression_1000_01.31.26_03.01PM.RData",
  "output/8_2030rnc_8females_depression_1000_01.31.26_03.19PM.RData",
  "output/9_2030rnc_9females_depression_1000_01.31.26_03.38PM.RData",
  "output/10_2030rnc_10females_depression_1000_01.31.26_03.56PM.RData",
  "output/11_2030rnc_11females_depression_1000_01.31.26_05.22PM.RData",
  "output/12_2030rnc_12females_depression_1000_01.31.26_05.55PM.RData",
  "output/13_2030rnc_13females_depression_1000_01.31.26_06.14PM.RData",
  "output/14_2030rnc_14females_depression_1000_01.31.26_06.32PM.RData",
  "output/15_2030rnc_15females_depression_1000_01.31.26_07.22PM.RData",
  "output/16_2030rnc_16females_depression_1000_01.31.26_08.08PM.RData",
  "output/17_2030rnc_17females_depression_1000_01.31.26_10.05PM.RData",
  "output/18_2030rnc_18females_depression_1000_01.31.26_10.23PM.RData",
  "output/19_2030rnc_19females_depression_1000_01.31.26_10.41PM.RData",
  "output/20_2030rnc_20females_depression_1000_01.31.26_10.59PM.RData"
  )}
  }else if(policyyear==2029){policylabel="2029"
  if (gender==1){genderlabel="male"
  file_names <- c(
  #2029 MALES LIST:
  "output/1_2029rnc_1males_depression_1000_01.30.26_11.36AM.RData",
  "output/2_2029rnc_2males_depression_1000_01.30.26_12.00PM.RData",
  "output/3_2029rnc_3males_depression_1000_01.30.26_12.19PM.RData",
  "output/4_2029rnc_4males_depression_1000_01.30.26_12.38PM.RData",
  "output/5_2029rnc_5males_depression_1000_01.30.26_12.57PM.RData",
  "output/6_2029rnc_6males_depression_1000_01.30.26_01.16PM.RData",
  "output/7_2029rnc_7males_depression_1000_01.30.26_01.35PM.RData",
  "output/8_2029rnc_8males_depression_1000_01.30.26_01.54PM.RData",
  "output/9_2029rnc_9males_depression_1000_01.30.26_02.15PM.RData",
  "output/10_2029rnc_10males_depression_1000_01.30.26_02.34PM.RData",
  "output/11_2029rnc_11males_depression_1000_01.30.26_02.53PM.RData",
  "output/12_2029rnc_12males_depression_1000_01.30.26_03.56PM.RData",
  "output/13_2029rnc_13males_depression_1000_01.30.26_06.08PM.RData",
  "output/14_2029rnc_14males_depression_1000_01.30.26_06.26PM.RData",
  "output/15_2029rnc_15males_depression_1000_01.30.26_06.44PM.RData",
  "output/16_2029rnc_16males_depression_1000_01.30.26_07.02PM.RData",
  "output/17_2029rnc_17males_depression_1000_01.30.26_07.45PM.RData",
  "output/18_2029rnc_18males_depression_1000_01.30.26_08.03PM.RData",
  "output/19_2029rnc_19males_depression_1000_01.30.26_08.21PM.RData",
  "output/20_2029rnc_20males_depression_1000_01.30.26_09.27PM.RData"
  )}
  else{genderlabel="female"
  file_names <- c(
  #2029 FEMALES LIST:
  "output/1_2029rnc_1females_depression_1000_01.30.26_11.27AM.RData",
  "output/2_2029rnc_2females_depression_1000_01.30.26_11.51AM.RData",
  "output/3_2029rnc_3females_depression_1000_01.30.26_12.09PM.RData",
  "output/4_2029rnc_4females_depression_1000_01.30.26_12.28PM.RData",
  "output/5_2029rnc_5females_depression_1000_01.30.26_12.47PM.RData",
  "output/6_2029rnc_6females_depression_1000_01.30.26_01.07PM.RData",
  "output/7_2029rnc_7females_depression_1000_01.30.26_01.26PM.RData",
  "output/8_2029rnc_8females_depression_1000_01.30.26_01.45PM.RData",
  "output/9_2029rnc_9females_depression_1000_01.30.26_02.05PM.RData",
  "output/10_2029rnc_10females_depression_1000_01.30.26_02.24PM.RData",
  "output/11_2029rnc_11females_depression_1000_01.30.26_02.43PM.RData",
  "output/12_2029rnc_12females_depression_1000_01.30.26_03.02PM.RData",
  "output/13_2029rnc_13females_depression_1000_01.30.26_05.06PM.RData",
  "output/14_2029rnc_14females_depression_1000_01.30.26_06.17PM.RData",
  "output/15_2029rnc_15females_depression_1000_01.30.26_06.35PM.RData",
  "output/16_2029rnc_16females_depression_1000_01.30.26_06.53PM.RData",
  "output/17_2029rnc_17females_depression_1000_01.30.26_07.36PM.RData",
  "output/18_2029rnc_18females_depression_1000_01.30.26_07.54PM.RData",
  "output/19_2029rnc_19females_depression_1000_01.30.26_08.12PM.RData",
  "output/20_2029rnc_20females_depression_1000_01.30.26_08.30PM.RData"
  )}
  }else if(policyyear==2027){policylabel="2027"
  if (gender==1){genderlabel="male"
  file_names <- c(
    #2027 MALES LIST:
    "output/1_2027rnc_1males_depression_1000_01.29.26_10.09AM.RData",
  "output/2_2027rnc_2males_depression_1000_01.29.26_10.28AM.RData",
  "output/3_2027rnc_3males_depression_1000_01.29.26_10.50AM.RData",
  "output/4_2027rnc_4males_depression_1000_01.29.26_11.10AM.RData",
  "output/5_2027rnc_5males_depression_1000_01.29.26_11.28AM.RData",
  "output/6_2027rnc_6males_depression_1000_01.29.26_11.46AM.RData",
  "output/7_2027rnc_7males_depression_1000_01.29.26_12.04PM.RData",
  "output/8_2027rnc_8males_depression_1000_01.29.26_01.03PM.RData",
  "output/9_2027rnc_9males_depression_1000_01.29.26_01.21PM.RData",
  "output/10_2027rnc_10males_depression_1000_01.29.26_01.40PM.RData",
  "output/11_2027rnc_11males_depression_1000_01.29.26_01.59PM.RData",
  "output/12_2027rnc_12males_depression_1000_01.29.26_03.17PM.RData",
  "output/13_2027rnc_13males_depression_1000_01.29.26_03.51PM.RData",
  "output/14_2027rnc_14males_depression_1000_01.29.26_04.10PM.RData",
  "output/15_2027rnc_15males_depression_1000_01.29.26_04.29PM.RData",
  "output/16_2027rnc_16males_depression_1000_01.29.26_04.48PM.RData",
  "output/17_2027rnc_17males_depression_1000_01.29.26_05.07PM.RData",
  "output/18_2027rnc_18males_depression_1000_01.29.26_05.26PM.RData",
  "output/19_2027rnc_19males_depression_1000_01.29.26_05.44PM.RData",
  "output/20_2027rnc_20males_depression_1000_01.29.26_07.01PM.RData"
  
  )}
  else{genderlabel="female"
  file_names <- c(
  #2027 FEMALES LIST:
    "output/1_2027rnc_1females_depression_1000_01.29.26_10.00AM.RData",
  "output/2_2027rnc_2females_depression_1000_01.29.26_10.19AM.RData",
  "output/3_2027rnc_3females_depression_1000_01.29.26_10.40AM.RData",
  "output/4_2027rnc_4females_depression_1000_01.29.26_11.00AM.RData",
  "output/5_2027rnc_5females_depression_1000_01.29.26_11.19AM.RData",
  "output/6_2027rnc_6females_depression_1000_01.29.26_11.37AM.RData",
  "output/7_2027rnc_7females_depression_1000_01.29.26_11.55AM.RData",
  "output/8_2027rnc_8females_depression_1000_01.29.26_12.13PM.RData",
  "output/9_2027rnc_9females_depression_1000_01.29.26_01.12PM.RData",
  "output/10_2027rnc_10females_depression_1000_01.29.26_01.31PM.RData",
  "output/11_2027rnc_11females_depression_1000_01.29.26_01.50PM.RData",
  "output/12_2027rnc_12females_depression_1000_01.29.26_02.23PM.RData",
  "output/13_2027rnc_13females_depression_1000_01.29.26_03.42PM.RData",
  "output/14_2027rnc_14females_depression_1000_01.29.26_04.00PM.RData",
  "output/15_2027rnc_15females_depression_1000_01.29.26_04.20PM.RData",
  "output/16_2027rnc_16females_depression_1000_01.29.26_04.38PM.RData",
  "output/17_2027rnc_17females_depression_1000_01.29.26_04.58PM.RData",
  "output/18_2027rnc_18females_depression_1000_01.29.26_05.17PM.RData",
  "output/19_2027rnc_19females_depression_1000_01.29.26_05.35PM.RData",
  "output/20_2027rnc_20females_depression_1000_01.29.26_06.43PM.RData"
  )}
  }else if(policyyear==2028){policylabel="2028"
  if (gender==1){genderlabel="male"
  file_names <- c(
  #2028 MALES LIST:
  "output/1_2028rnc_1males_depression_1000_01.29.26_07.57PM.RData",
  "output/2_2028rnc_2males_depression_1000_01.29.26_08.15PM.RData",
  "output/3_2028rnc_3males_depression_1000_01.29.26_08.36PM.RData",
  "output/4_2028rnc_4males_depression_1000_01.29.26_08.54PM.RData",
  "output/5_2028rnc_5males_depression_1000_01.29.26_10.13PM.RData",
  "output/6_2028rnc_6males_depression_1000_01.29.26_10.31PM.RData",
  "output/7_2028rnc_7males_depression_1000_01.29.26_10.49PM.RData",
  "output/8_2028rnc_8males_depression_1000_01.29.26_11.07PM.RData",
  "output/9_2028rnc_9males_depression_1000_01.30.26_12.17AM.RData",
  "output/10_2028rnc_10males_depression_1000_01.30.26_12.35AM.RData",
  "output/11_2028rnc_11males_depression_1000_01.30.26_12.54AM.RData",
  "output/12_2028rnc_12males_depression_1000_01.30.26_01.42AM.RData",
  "output/13_2028rnc_13males_depression_1000_01.30.26_02.00AM.RData",
  "output/14_2028rnc_14males_depression_1000_01.30.26_02.18AM.RData",
  "output/15_2028rnc_15males_depression_1000_01.30.26_02.37AM.RData",
  "output/16_2028rnc_16males_depression_1000_01.30.26_04.20AM.RData",
  "output/17_2028rnc_17males_depression_1000_01.30.26_05.08AM.RData",
  "output/18_2028rnc_18males_depression_1000_01.30.26_05.26AM.RData",
  "output/19_2028rnc_19males_depression_1000_01.30.26_05.44AM.RData",
  "output/20_2028rnc_20males_depression_1000_01.30.26_06.23AM.RData"
  )}
  else{genderlabel="female"
  file_names <- c(
  #2028 FEMALES LIST:
  "output/1_2028rnc_1females_depression_1000_01.29.26_07.48PM.RData",
  "output/2_2028rnc_2females_depression_1000_01.29.26_08.06PM.RData",
  "output/3_2028rnc_3females_depression_1000_01.29.26_08.24PM.RData",
  "output/4_2028rnc_4females_depression_1000_01.29.26_08.45PM.RData",
  "output/5_2028rnc_5females_depression_1000_01.29.26_09.03PM.RData",
  "output/6_2028rnc_6females_depression_1000_01.29.26_10.22PM.RData",
  "output/7_2028rnc_7females_depression_1000_01.29.26_10.40PM.RData",
  "output/8_2028rnc_8females_depression_1000_01.29.26_10.58PM.RData",
  "output/9_2028rnc_9females_depression_1000_01.29.26_11.53PM.RData",
  "output/10_2028rnc_10females_depression_1000_01.30.26_12.26AM.RData",
  "output/11_2028rnc_11females_depression_1000_01.30.26_12.44AM.RData",
  "output/12_2028rnc_12females_depression_1000_01.30.26_01.03AM.RData",
  "output/13_2028rnc_13females_depression_1000_01.30.26_01.51AM.RData",
  "output/14_2028rnc_14females_depression_1000_01.30.26_02.09AM.RData",
  "output/15_2028rnc_15females_depression_1000_01.30.26_02.27AM.RData",
  "output/16_2028rnc_16females_depression_1000_01.30.26_03.24AM.RData",
  "output/17_2028rnc_17females_depression_1000_01.30.26_04.59AM.RData",
  "output/18_2028rnc_18females_depression_1000_01.30.26_05.17AM.RData",
  "output/19_2028rnc_19females_depression_1000_01.30.26_05.35AM.RData",
  "output/20_2028rnc_20females_depression_1000_01.30.26_05.54AM.RData"
  )}
  }

# Initialize the results list
results_list <- list()

# Load each file and append the results to the results_list
for (file_name in file_names) {
  load(file_name)
  #load(paste0(mainDir,file_name))
  results_list <- append(results_list, list(
    list(l.results = l.results, l.results_D = l.results_D, l.results_ND = l.results_ND)
  ))
}

# Helper functions to combine elements

combine_model_prevs <- function(dfs) {
  combined_df <- Reduce(`+`, dfs) / length(dfs)
  return(combined_df)
}

combine_vectors <- function(vectors) {
  combined_vec <- Reduce(`+`, vectors) / length(vectors)
  return(combined_vec)
}

combine_data_frames <- function(dfs) {
  combined_df <- Reduce(`+`, dfs) / length(dfs)
  return(combined_df)
}

combine_result_elements <- function(result_elements) {
  combined <- list()
  
  combined[["l.model_prevs"]] <- setNames(
    lapply(names(result_elements[[1]][["l.model_prevs"]]), function(pop) {
      combine_model_prevs(lapply(result_elements, function(res) res[["l.model_prevs"]][[pop]]))
    }),
    names(result_elements[[1]][["l.model_prevs"]])
  )
  
  combined[["m.cuw"]] <- combine_data_frames(lapply(result_elements, `[[`, "m.cuw"))
  combined[["v.lifeyears"]] <- combine_vectors(lapply(result_elements, `[[`, "v.lifeyears"))
  combined[["v.SAD_old"]] <- combine_vectors(lapply(result_elements, `[[`, "v.SAD_old"))
  combined[["v.yll_old"]] <- combine_vectors(lapply(result_elements, `[[`, "v.yll_old"))
  combined[["v.SAD_old_disc"]] <- combine_vectors(lapply(result_elements, `[[`, "v.SAD_old_disc"))
  combined[["v.yll_old_disc"]] <- combine_vectors(lapply(result_elements, `[[`, "v.yll_old_disc"))
  combined[["v.VAD_old"]] <- combine_vectors(lapply(result_elements, `[[`, "v.VAD_old"))
  combined[["v.VAD_old_disc"]] <- combine_vectors(lapply(result_elements, `[[`, "v.VAD_old_disc"))
  combined[["v.lifeyears_pop_new"]] <- combine_vectors(lapply(result_elements, `[[`, "v.lifeyears_pop_new"))
  combined[["v.SAD_new"]] <- combine_vectors(lapply(result_elements, `[[`, "v.SAD_new"))
  combined[["v.lifeyears_pop_new_disc"]] <- combine_vectors(lapply(result_elements, `[[`, "v.lifeyears_pop_new_disc"))
  combined[["v.SAD_new_disc"]] <- combine_vectors(lapply(result_elements, `[[`, "v.SAD_new_disc"))
  combined[["m.prev_C"]] <- combine_data_frames(lapply(result_elements, `[[`, "m.prev_C"))
  combined[["m.prev_F"]] <- combine_data_frames(lapply(result_elements, `[[`, "m.prev_F"))
  combined[["m.prev_N"]] <- combine_data_frames(lapply(result_elements, `[[`, "m.prev_N"))
  combined[["init"]] <- combine_data_frames(lapply(result_elements, `[[`, "init"))
  combined[["cess"]] <- combine_data_frames(lapply(result_elements, `[[`, "cess"))
  combined[["v.deathrate"]] <- combine_vectors(lapply(result_elements, `[[`, "v.deathrate"))

  
  return(combined)
}

combine_results_by_scenario <- function(results_list, scenario_name) {
  combined_results <- list()
  
  results_elements <- lapply(results_list, function(res) res[[scenario_name]])
  combined_results <- combine_result_elements(results_elements)
  
  return(combined_results)
}

combine_all_results <- function(results_list) {
  combined_l_results <- list()
  combined_l_results_D <- list()
  combined_l_results_ND <- list()
  
  scenarios <- names(results_list[[1]][["l.results"]])
  
  for (scenario in scenarios) {
    combined_l_results[[scenario]] <- combine_results_by_scenario(lapply(results_list, `[[`, "l.results"), scenario)
    combined_l_results_D[[scenario]] <- combine_results_by_scenario(lapply(results_list, `[[`, "l.results_D"), scenario)
    combined_l_results_ND[[scenario]] <- combine_results_by_scenario(lapply(results_list, `[[`, "l.results_ND"), scenario)
  }
  
  combined_results <- list(combined_l_results = combined_l_results, combined_l_results_D = combined_l_results_D, combined_l_results_ND = combined_l_results_ND)
  return(combined_results)
}

# Combine all results
combined_results <- combine_all_results(results_list)

l.results <- combined_results$combined_l_results
l.results_D <- combined_results$combined_l_results_D
l.results_ND <- combined_results$combined_l_results_ND

n=1000*length(results_list)
# Save the combined results
save(l.results, l.results_D, l.results_ND,
     file = paste0("output/combined_",policylabel,"_",genderlabel,n,".RData"))

