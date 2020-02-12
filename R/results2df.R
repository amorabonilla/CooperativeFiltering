rowpref2df <-function(num_execution,
                      num_experiment,
                      dt_experiment_pref,
                      iteration,
                      user,
                      pref,
                      grad){
    long <- length(pref)
    dt_exper1 <- data.frame(execution=rep(num_execution,long),
                            experiment= rep(num_experiment,long),
                            iteration=rep(iteration,long),
                                   user=rep(user,long),
                                   attribute=pref[[1]],
                                   grade=grad[[1]])
    dt_experiment_pref <- rbind(dt_experiment_pref,dt_exper1)
  
  return(dt_experiment_pref) 
}
  
preferences2df <- function(num_execution,
                           num_experiment,
                           n_iteration,
                           dt_experiment_pref,
                           dt_preferences){
    n <- dim(dt_preferences)[1]
    for (k in seq_len(n)){
        user <-dt_preferences$user[k]
        pref <- dt_preferences$preferences[k]
        grad <-dt_preferences$grades[k]
        dt_experiment_pref <- rowpref2df(num_execution,num_experiment,
                                         dt_experiment_pref,
                                         n_iteration,user,pref,grad)
    }#end for
  
  return(dt_experiment_pref) 
}#end function

 
 


agregation2df <- function(attributes,
                          num_execution,
                          num_experiment,
                          num_iteration,
                          next_lhs_minimal_sets,
                          closure,
                          dt_agregation_pref,
                          dt_attributes){
#  browser()
  if (is.null(next_lhs_minimal_sets)) return(dt_agregation_pref) 
  
  my_closure <-  attributes[as.logical(closure$get_vector())]
  
  attrib <- as.character(set_union(as.set(next_lhs_minimal_sets),as.set(my_closure)))
 # attrib <- c(next_lhs_minimal_sets,closure)
  n <- length(attrib)
  dt_agreg1 <- data.frame(execution=rep(num_execution,n),
                          experiment= rep(num_experiment,n),
                          iteration=rep(num_iteration,n),
                          attribute=attrib)
 

  
 
  dt_agreg1 <- dt_agreg1 %>%
            mutate(grade=sapply(attrib, 
                                 function(x){ 
                                   ind <- dt_attributes$attribute==x
                                   unlist(dt_attributes$grade[ind])
                                  })) %>%
            mutate(InClosure=sapply(attribute, 
                        function(x) 
                           x %in% my_closure)) 
 
  
  dt_agregation_pref <- rbind(dt_agregation_pref,dt_agreg1)
  
  return(dt_agregation_pref) 
  }#end function  


 
final_results2df <- function(num_execution,
                       num_experiment,
                       n_iteration,
                       closure){
  
  n <- dim(dt_preferences)[1]
  for (k in seq_len(n)){
    user <-dt_preferences$user[k]
    pref <- dt_preferences$preferences[k]
    grad <-dt_preferences$grades[k]
    dt_experiment_pref <- rowpref2df(num_execution,num_experiment,
                                     dt_experiment_pref,
                                     n_iteration,user,pref,grad)
  }#end for
  
 # dt_results <- () 
}#end function
 
