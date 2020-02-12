# Implementation for a random experiment
random_filtering_features <- function(FC,fcaR_fc,
                                      Att,
                                      #Rules,
                                      dt_preferences) {
  IS <- fcaR_fc$implications
  Att_FC <-fcaR_fc$attributes
  #implication_set$new(name="IS1",Att)
#  IS$from_arules(Rules)

  if (IS$cardinality()== 0 || length(Att)==0)
    return(list("objects"=NULL) ,
           "next_attributes"=NULL)
  #  my_obj  <- sample(1:length(rownames(FC)),4)

#  dt_attributes <- data.frame(attributes=Att,agregated_degree=rep(0,1,length(Att)))
  dt_attributes <- data.frame(attributes=Att,
                              agregated_degree=rep(0,1,length(Att)))
  rownames(dt_attributes) <- Att

  for (l in seq_len(length(Att))){
    dt_attributes$agregated_degree[l] <- list(0) }

  for (k in seq_len(length(dt_preferences$user))){
    for (l in seq_len(length(Att))){
      dt_attributes$agregated_degree[[l]] <- list(unlist(dt_attributes$agregated_degree[[l]]),
                                                  count_degree_for_attribute(Att[l],dt_preferences$preferences[[k]],
                                                                             dt_preferences$grades[[k]]))

    }#end for
  }#end for
  # first OWA
  dt_attributes <- agregation1(Att,dt_attributes)

# browser()
  Next <- agregation2(IS, Att_FC,dt_attributes)
 # browser()
  # fcaR_fc1 <- FormalContext$new(FC)
  # fcaR_fc1$add_implications(Next$Rules)
  # fcaR_fc1$implications$apply_rules(rules = c("composition","simplification"))

  if ( Next$Rules$cardinality()==0)
    return(list("objects"=intent(FC,Next$closure),
                "next_attributes"=NULL,
                "new_fcaR_fc"=NULL,
                "closure"=Next$closure))


 # browser()
  fcaR_fc$implications$initialize(attributes=Next$Rules$get_attributes())
  fcaR_fc$implications$add(Next$Rules)  # actualizo las reglas
  fcaR_fc$implications$apply_rules(rules = c("composition"))
                                             #,"generalization"))
                                             #"simplification"))

  if (fcaR_fc$implications$cardinality()==1){
#    browser()
    next_lhs_minimal_sets <- attributes_from_minimals(fcaR_fc$implications$get_LHS_matrix())}
  else
    next_lhs_minimal_sets  <- attributes_from_minimals(lhs.minimals(fcaR_fc$implications))

  if ( length(next_lhs_minimal_sets)==0)
    return(list("evaluated_attributes"=dt_attributes$attributes[dt_attributes$agregated_degree>0],
          "objects"=intent(FC,Next$closure) ,
                "next_attributes"=NULL,
                "new_fcaR_fc"=fcaR_fc,
                "closure"=Next$closure))

  return(list("evaluated_attributes"=dt_attributes$attributes[dt_attributes$agregated_degree>0],
              "objects"=intent(FC,Next$closure) ,
              "next_attributes"=next_lhs_minimal_sets,
              "new_fcaR_fc"=fcaR_fc,
              "closure"=Next$closure))
}#end filtering





count_degree_for_attribute <- function(Attr1,list_pref1,list_degree1){
  for (k in seq_len(length(list_pref1))){
    if (Attr1== list_pref1[k]){
      return(list_degree1[k])
    }
  }
  return(0)
}# end function


