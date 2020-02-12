#sink("salida1_player3.txt")
name_file_rules <- "reglas3.xml"
num_execution <- 1
num_experiment <- 1
verbose <- TRUE

FC  <- importing_dataset(FALSE, dataset_selected = 'datasets/context.csv')
FC
# n number of users
n_users=5
num_scales <- 4
my_support <- 0.01

mining_in_table(FC,my_support,name_file_rules)


recommendation <- random_experiment_dataset(FC,name_file_rules,
                                            n_users,num_scales,
                                            my_support, 
                                            num_execution,
                                            num_experiment, verbose )

 