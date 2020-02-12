source("R/multmerge.R")
name_global_experiment_sizes <-stringr::str_c("output/",
                                                 "global_sizes.csv")
name_global_experiment_pref_users <-stringr::str_c("output/",
                                             "global_pref_users.csv")
name_global_agregation_pref_agregated <-stringr::str_c("output/",
                                         "global_pref_agregated.csv")
name_global_experiment_pref_sizes <-stringr::str_c("output/",
                                          "global_pref_sizes.csv")
name_global_experiment_closures <-stringr::str_c("output/",
                                                   "global_closure.csv")

exp_sizes = multmerge("output/experiment_sizes")
exp_pref_sizes = multmerge("output/experiment_pref_sizes")
exp_pref_users = multmerge("output/pref_users")
exp_pref_agregated = multmerge("output/experiment_agregated")
exp_pref_closures = multmerge("output/experiment_closure")


write.csv(exp_sizes,
          name_global_experiment_sizes, row.names = FALSE)
write.csv(exp_pref_sizes,
          name_global_experiment_pref_sizes, row.names = FALSE)
write.csv(exp_pref_users,
          name_global_experiment_pref_users, row.names = FALSE)
write.csv(exp_pref_agregated,
          name_global_agregation_pref_agregated, row.names = FALSE)
write.csv(exp_pref_closures,
          name_global_experiment_closures, row.names = FALSE)


