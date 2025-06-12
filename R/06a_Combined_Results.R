#specify the output files you want to combine:
gender=1 #1 for male and 2 for female

if (gender==1){genderlabel="male"
file_names <- c(
  #males
  "output/rnc_1males_depression_10000_05.06.25_08.06AM.RData",
  "output/rnc_2males_depression_10000_05.04.25_09.34PM.RData"#,
)
}else{genderlabel="female"
file_names <- c(
  #males
  "output/rnc_1females_depression_10000_05.05.25_09.14PM.RData",
  "output/rnc_2females_depression_10000_05.04.25_10.37AM.RData"#,
)
}

# Initialize the results list
results_list <- list()

# Load each file and append the results to the results_list
for (file_name in file_names) {
  load(file_name)
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

n=10000*length(results_list)
# Save the combined results
save(l.results, l.results_D, l.results_ND,
     file = paste0("output/combined_",genderlabel,n,".RData"))

