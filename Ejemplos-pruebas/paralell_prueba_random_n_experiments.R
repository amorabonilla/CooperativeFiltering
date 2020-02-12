source("R/main.R")

nombre_experiment <- "prueba4_"
namefile <- stringr::str_c("output/",nombre_experiment,".txt")
num_execution <-1
 
#capture.output({
n_users=5
num_scales <- 4
my_support <- 0.1  
spar_degree <- 0.5

num_objects_ini <- 20
num_objects_fin <- 30
num_attributes_ini <- 20
num_attributes_fin <- 30

num <- detectCores()
NumberOfCluster <- num/2
 

exp_time <- system.time( 
               paralell_experiment_recommendation(NumberOfCluster,
                                                  num_execution,
                                                  nombre_experiment,
                                                  num_objects_ini,
                                                  num_objects_fin,
                            num_attributes_ini,num_attributes_fin,
                            spar_degree,
                            n_users,
                            num_scales,my_support))
          
cat("***************\n")
cat(">> Tiempo total: ",exp_time,"\n")

#},file=namefile)#end capture
# append=TRUE)

 