maindep<-l.results$main[["l.model_prevs"]][["D"]][97:192,]
baselinedep<-l.results$baseline[["l.model_prevs"]][["D"]][97:192,]

ggplot() +
  # Add the first dataset
  geom_line(data = maindep, aes(x = year, y = prev), color = "blue") +
  # Add the second dataset
  geom_line(data = baselinedep, aes(x = year, y = prev), color = "black")# +
# Add labels