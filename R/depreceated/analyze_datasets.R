
#max_size <-dt_experiment_sizes$sigma[1]

#max_attributes <-dt_experiment_sizes$attributes[1]

#max_objects <-dt_experiment_sizes$objects[1]


summary_by_iteration <- dt_experiment_sizes %>%
  rename(step =iteration) %>%
  group_by(step) %>%
  summarise(
    n = n(),
 #   mean_size_implications = mean(sigma, na.rm = TRUE),
#    mean_attributes = mean(attributes, na.rm = TRUE),
    mean_objects = mean(objects, na.rm = TRUE),
  #  max_size_implications = max(sigma, na.rm = TRUE),
  #  max_attributes = max(attributes, na.rm = TRUE),
    max_objects = max(objects, na.rm = TRUE),
  )  %>%
  mutate(prunning_objects = 100-(mean_objects/.GlobalEnv$max_objects)*100)






