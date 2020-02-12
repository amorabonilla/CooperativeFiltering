#sink("salida1_random_50.txt")
n_users=5
num_scales <- 4
my_support <- 0.2

num_objects <- 70
num_attributes <- 70
spar_degree <- 0.5
num_experiment <- 1 

t <-system.time(pref <- random_experiment(num_experiment,
                                          num_objects,
                                          num_attributes,
                                          spar_degree,
                                          n_users,
                                          num_scales,
                                          my_support)
            )
cat("Tiempo\n")
cat(t)
cat("Preferencias\n")

pref
