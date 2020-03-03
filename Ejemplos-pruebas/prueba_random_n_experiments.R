library(devtools)
devtools::load_all()
library(knitr)
library(arules)
library(fcaR)
library(Matrix)


library(tidyverse)
library(pmml)
library(sets)
library(magrittr)
# library("stringr")
# library(data.table)
# library(readxl)
# #library(readr)

library(foreach)
library(doParallel)
library(doSNOW)

nombre_experiment <- "feb2020_1_"
num_execution <- 9
namefile <- stringr::str_c("output/",nombre_experiment,".txt")

capture.output({
n_users=3
num_scales <- 4
my_support <- 0.1
spar_degree <- 0.45

num_objects_ini <- 20
num_objects_fin <- 500
num_attributes_ini <- 20
num_attributes_fin <- 500


exp_time <- system.time( random_experiments(num_execution,nombre_experiment,num_objects_ini,num_objects_fin,
                            num_attributes_ini,num_attributes_fin,
                            spar_degree,n_users,num_scales,my_support))

cat("***************\n")
cat(">> Tiempo total: ",exp_time,"\n")

},file=namefile)#end capture
# append=TRUE)

