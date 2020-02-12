
random_mining_in_table <- function(FC,my_support){
  FC1 <-as.data.frame(sapply(FC, function(x) as.logical(x)))
  rule_param = list(
    supp =my_support,
    conf = 1,
    minlen= 2, 
    maxlen = 40
  )
  Rules <- apriori(FC1,parameter = rule_param)

  
   Rules <- Rules[!is.redundant(Rules)]
  fcaR_fc <- FormalContext$new(FC1)
  fcaR_fc$implications$add(Rules)
  fcaR_fc$implications
  fcaR_fc$implications$apply_rules(rules = c("composition"))
                                       #      "generalization"))
                                   #,"simplification"))
  
  #  write.PMML(Rules, file = name_file_rules)  
  return(fcaR_fc)
}




mining_in_table <- function(FC,my_support,name_file_rules){
  FC1 <-as.data.frame(sapply(FC, function(x) as.logical(x)))
  Rules <- apriori(FC1,parameter = list(confidence=1,support=my_support,minlen=2))
  Rules <- Rules[!is.redundant(Rules)]
  fcaR_fc <- FormalContext$new(FC)
  fcaR_fc$implications$add(Rules)
  fcaR_fc$implications
  fcaR_fc$implications$apply_rules(rules = c("composition", "generalization", "simplification"))
  Rules <- fcaR_fc$implications$to_arules(quality=FALSE)
  write.PMML(Rules, file = name_file_rules)
  return(fcaR_fc)
}