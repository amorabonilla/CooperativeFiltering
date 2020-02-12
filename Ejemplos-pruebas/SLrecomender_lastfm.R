name_file_rules <- "reglas_lastfm_novale.xml"
num_execution <- 1
num_experiment <- 1
verbose <- TRUE

# n number of users
n_users=5
num_scales <- 4
my_support <- 0.31

FC  <- importing_dataset(FALSE, dataset_selected = 'datasets/lastfm.csv')
FC <- t(FC)


#mining_in_table(FC,my_support,name_file_rules)


fc_lastfm <- FormalContext$new(FC)
fc_lastfm$find_implications() 


recommendation <- random_experiment_dataset(FC,name_file_rules,
                                            n_users,num_scales,
                                            my_support, 
                                            num_execution,
                                            num_experiment, verbose )

