#specify the output files you want to combine:
file_names <- c(
  #males
  "output/rnc_9males_depression_10000_04.05.25_08.44PM.RData",
  "output/rnc_6males_depression_10000_04.05.25_06.16PM.RData",
  "output/rnc_3males_depression_10000_04.05.25_02.05PM.RData",
  "output/rnc_255males_depression_10000_04.04.25_02.27PM.RData"
  #females
  
)

# Initialize the results list
results_list <- list()

# Load each file and append the results to the results_list
for (file_name in file_names) {
  load(file_name)
  results_list <- append(results_list, list(
    list(l.results = l.results, l.results_D = l.results_D, l.results_ND = l.results_ND)
  ))
}

# Add more files to the results_list as needed
# load("output/rnc_Xmales_depression_10000_XX.XX.XX_XX.XXPM.RData")
# results_list <- append(results_list, list(
#   list(l.results = l.results, l.results_D = l.results_D, l.results_ND = l.results_ND)
# ))

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
  combined[["v.SAD"]] <- combine_vectors(lapply(result_elements, `[[`, "v.SAD"))
  combined[["v.yll"]] <- combine_vectors(lapply(result_elements, `[[`, "v.yll"))
  combined[["m.prev_C"]] <- combine_data_frames(lapply(result_elements, `[[`, "m.prev_C"))
  combined[["m.prev_F"]] <- combine_data_frames(lapply(result_elements, `[[`, "m.prev_F"))
  combined[["m.prev_N"]] <- combine_data_frames(lapply(result_elements, `[[`, "m.prev_N"))
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

combined_l_results <- combined_results$combined_l_results
combined_l_results_D <- combined_results$combined_l_results_D
combined_l_results_ND <- combined_results$combined_l_results_ND

# Save the combined results
# save(combined_l_results, combined_l_results_D, combined_l_results_ND, 
#      file = "output/combined_males_depression_results.RData")

maindep<-combined_l_results$main[["l.model_prevs"]][["D"]][97:192,]
baselinedep<-combined_l_results$baseline[["l.model_prevs"]][["D"]][97:192,]

ggplot() +
  # Add the first dataset
  geom_line(data = maindep, aes(x = year, y = prev), color = "blue") +
  # Add the second dataset
  geom_line(data = baselinedep, aes(x = year, y = prev), color = "black")# +
# Add labels


# Save the combined results
save(combined_l_results, combined_l_results_D, combined_l_results_ND, 
     file = "output/combined_males_depression_results.RData")