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
# library(foreach)

#read_chunk("R/main.R")


# setwd("/Volumes/GoogleDrive/Mi unidad/CooperativeFiltering")

FC <- read.csv("/Volumes/GoogleDrive/Mi\ unidad/CooperativeFiltering/datasets/sytora-master/data/all-files-for-ml/all_pivoted.csv")


colnames(FC)


rownames(FC) <- FC$Disease
filas <- rownames(FC)

FC$Disease <- NULL

FC1 <-as.data.frame(sapply(FC, function(x) as.logical(x)))
#View(head(FC1))
rownames(FC1) <- filas

my_support <- 0.01
rule_param = list(
  supp = my_support,
  conf = 1,
  maxlen = 15
)

Rules <- apriori(FC1,parameter = rule_param)
name_file_rules <- "reglas_disease.xml"



Rules1 <- Rules[!is.redundant(Rules)]


dt_experiment_sizes <- data.frame(execution=c(),
                                  experiment=c(),
                                  iteration=c(),
                                  sigma=c(),
                                  #closure=c(),
                                  attributes=c(),
                                  objects=c(),
                                  closure=c())







#fc_movielens$implications
#fc_movielens$implications$apply_rules(rules = c("composition",
#                                                "simplification"))

#Rules <- fc_movielens$implications$to_arules(quality = FALSE)

#write.PMML(Rules, file = name_file_rules)

num_execution <- 1
num_experiment <- 1
verbose <- TRUE

# n number of users
n_users=2
num_scales <- 4

number_executions_of_the_experiment <- 20





for (k in 1:number_executions_of_the_experiment){

Rules <- Rules1

#  write.PMML(Rules, file = name_file_rules)



  recommendation <- random_experiment_dataset(Rules,FC1,name_file_rules,
                                              n_users,num_scales,
                                              my_support,
                                              num_execution,
                                              num_experiment, verbose )

  dt_experiment_sizes <- rbind(dt_experiment_sizes,recommendation$pref_sizes )
  num_experiment <- num_experiment +1

}
dt_experiment_sizes$execution <- NULL
dt_experiment_sizes
