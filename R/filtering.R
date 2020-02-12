#' n users 
#' k --> top_k features
 


# Implementation for the web

filtering_features <- function(fcaR_fc,
                               Att,
                               #Rules,
                               dt_preferences) {
#  Rules <- read.PMML(file = name_file_rules)
  Att <- colnames(FC)
  IS <- implication_set$new(name="IS1",Att) 
  IS$from_arules(Rules)
  if (IS$cardinality()== 0)
    return(list("objects"=NULL) ,
                "next_attributes"=NULL)
  #  my_obj  <- sample(1:length(rownames(FC)),4)
    dt_preferences <- data.frame(user=unlist(list_users))
    dt_preferences <- 
      dt_preferences %>%
      mutate(preferences =list_preferences) %>%
      mutate(grades=list_grades)
    
    dt_attributes <- data.frame(attributes=Att,agregated_degree=rep(0,1,length(Att)))  
    for (l in seq_len(length(Att))){
      dt_attributes$agregated_degree[l] <- list(0) }
      
    for (k in seq_len(length(list_users[[1]]))){
      for (l in seq_len(length(Att))){
        dt_attributes$agregated_degree[[l]] <- list(unlist(dt_attributes$agregated_degree[[l]]),
              count_degree_for_attribute(Att[l],dt_preferences$preferences[[k]],
                                                dt_preferences$grades[[k]]))
        
      }#end for
    }#end for
    # first OWA
    
    dt_attributes <- agregation1(Att,dt_attributes)
    # cambiado Rules por IS
    Next <- agregation2(IS, Att,dt_attributes)
    next_lhs_minimal_sets  <- attributes_from_minimals(lhs.minimals(Next$Rules))
 #   write.PMML(Next$Rules, file = name_file_rules)
    
  return(list("objects"=intent(FC,Next$closure) ,
              "next_attributes"=Att[next_lhs_minimal_sets@data@i+1],
              "next_rules"=Next$Rules))
}#end filtering

 
      
count_degree_for_attribute <- function(Attr1,list_pref1,list_degree1){
  for (k in seq_len(length(list_pref1))){
    if (Attr1== list_pref1[k]){
      return(list_degree1[k])
    }
  }
  return(0)
}# end function      
 


count_degree_for_attribute <- function(Attr1,list_pref1,list_degree1){
  for (k in seq_len(length(list_pref1))){
    if (Attr1== list_pref1[k]){
      return(list_degree1[k])
    }
  }
  return(0)
}# end function    