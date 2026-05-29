
rm(list = ls()) 

# Set working directory
mainDir = "/Users/srs475/Library/CloudStorage/Dropbox-UniversityofMichigan/Sarah Skolnick/GitHub/mdse-microsim/"

setwd(mainDir)

# specify the output files you want to combine:
gender <- 2      # 1 for male and 2 for female
policyyear <- 2027
genderlabel <- ifelse(gender == 1, "male", "female")

if (gender==1){
  file_names <- c(
    #males
    "output/1_2027rnc_1males_depression_10000_05.27.26_03.18AM.RData",
    "output/2_2027rnc_2males_depression_10000_05.28.26_12.48AM.RData"
  )
} else {
  file_names <- c(
    #females
    "output/1_2027rnc_1females_depression_10000_05.26.26_07.37PM.RData",
    "output/2_2027rnc_2females_depression_10000_05.27.26_04.41PM.RData"
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
combine_model_prevs <- function(dfs) Reduce(`+`, dfs) / length(dfs)
combine_vectors     <- function(vectors) Reduce(`+`, vectors) / length(vectors)
combine_data_frames <- function(dfs) Reduce(`+`, dfs) / length(dfs)

combine_result_elements <- function(result_elements) {
  combined <- list()
  
  combined[["l.model_prevs"]] <- setNames(
    lapply(names(result_elements[[1]][["l.model_prevs"]]), function(pop) {
      combine_model_prevs(lapply(result_elements, function(res) res[["l.model_prevs"]][[pop]]))
    }),
    names(result_elements[[1]][["l.model_prevs"]])
  )
  
  vec_fields <- c("v.lifeyears", "v.SAD_old", "v.yll_old", "v.SAD_old_disc", "v.yll_old_disc",
                  "v.VAD_old", "v.VAD_old_disc", "v.lifeyears_pop_new", "v.SAD_new",
                  "v.lifeyears_pop_new_disc", "v.SAD_new_disc", "v.deathrate")
  df_fields  <- c("m.cuw", "m.prev_C", "m.prev_F", "m.prev_N", "init", "cess")
  
  for (f in vec_fields) combined[[f]] <- combine_vectors(lapply(result_elements, `[[`, f))
  for (f in df_fields)  combined[[f]] <- combine_data_frames(lapply(result_elements, `[[`, f))
  
  return(combined)
}

combine_all_results <- function(results_list) {
  scenarios <- names(results_list[[1]][["l.results"]])
  
  combine_scenario <- function(slot) {
    setNames(
      lapply(scenarios, function(s) combine_result_elements(lapply(results_list, function(r) r[[slot]][[s]]))),
      scenarios
    )
  }
  
  list(
    combined_l_results    = combine_scenario("l.results"),
    combined_l_results_D  = combine_scenario("l.results_D"),
    combined_l_results_ND = combine_scenario("l.results_ND")
  )
}

# Combine all results
combined_results <- combine_all_results(results_list)

l.results    <- combined_results$combined_l_results
l.results_D  <- combined_results$combined_l_results_D
l.results_ND <- combined_results$combined_l_results_ND

n <- 10000 * length(results_list)

save(l.results, l.results_D, l.results_ND,
     file = paste0("output/combined_", policyyear, "_", genderlabel, n, ".RData"))
