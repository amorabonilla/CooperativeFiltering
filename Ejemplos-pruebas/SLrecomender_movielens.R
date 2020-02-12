library(recommenderlab) 
library(ggplot2) 
library(data.table)
library(reshape2)
data(MovieLense)

df_MovielLense <- as(MovieLense,"data.frame")
FC <- as(split(df_MovielLense[,"item"], df_MovielLense[,"user"]), "transactions")
colnames(FC)
rownames(FC)

my_support <- 0.1
rule_param = list(
  supp = my_support,
  conf = 1,
  maxlen = 15
)

Rules <- apriori(FC,parameter = rule_param)
name_file_rules <- "reglas_movielens_novale.xml"



Rules <- Rules[!is.redundant(Rules)]
Rules
fc_movielens <- FormalContext$new(FC)
fc_movielens$implications$add(Rules)
fc_movielens$implications
fc_movielens$implications$apply_rules(rules = c("composition",
                                                "simplification"))

Rules <- fc_movielens$implications$to_arules(quality = FALSE)

write.PMML(Rules, file = name_file_rules)

num_execution <- 1
num_experiment <- 1
verbose <- TRUE

# n number of users
n_users=5
num_scales <- 4


recommendation <- random_experiment_dataset(FC,name_file_rules,
                                            n_users,num_scales,
                                            my_support, 
                                            num_execution,
                                            num_experiment, verbose )

