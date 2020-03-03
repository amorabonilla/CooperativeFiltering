random_experiment <- function(num_execution,
                              num_experiment,
                              num_objects,
                              num_attributes,
                              spar_degree,
                              n_users,
                              num_scales,
                              my_support,
                              verbose=TRUE ){
  # to save results and initialization
  dt <- initialize.df()
  dt_closure  <- dt$closure
  dt_experiment_pref <- dt$pref
  dt_agregation_pref <- dt$agregation_preg
  dt_experiment_sizes <- dt$sizes
  closure_accumulated <- c()
  n_iteration <- 0
#  browser()
  # reading the FC
  FC <- importing_dataset(random=TRUE,num_objects,num_attributes,
                          sparness_degree = spar_degree)
  objects_global=rownames(FC)

  # mining
  fcaR_fc <- random_mining_in_table(FC,my_support)

  next_attributes <- fcaR_fc$attributes
  while( no_end_condition_experiment(next_attributes,
                                      fcaR_fc,
                                      objects_global)
#estas restricciones fuertes, 1, y 0 valores que habia
        ){#begining of the while

    n_iteration  <- n_iteration +1
    dt_preferences <- user_preferences(next_attributes,n_users,num_scales)

    # SAVING IN DATA.FRAME PREFERENCES
    dt_experiment_pref <- preferences2df(num_execution,
                                         num_experiment,
                                         n_iteration,
                                         dt_experiment_pref,
                                         dt_preferences)
    # END SAVING

    # SAVING IN DATA.FRAME SIZES
   # browser()
    dt_experiment1_sizes <- data.frame(execution=num_execution,
                                       experiment=num_experiment,
                                       iteration=c(n_iteration),
                                       sigma=c(fcaR_fc$implications$cardinality()),
                                       attributes=c(length(next_attributes)),
                                       objects=length(objects_global))
    dt_experiment_sizes <- rbind(dt_experiment_sizes,dt_experiment1_sizes)

#    browser()    # END SAVING
    recommendation <- random_filtering_features(FC,fcaR_fc,#name_file_rules,
                                                next_attributes,
                                                #Rules,
                                                #list_users,
                                                dt_preferences)
#    if (is.null(recommendation$new_fcaR_fc))
#        browser()
#    if (is.null(fcaR_fc)) {


#browser()
    if (n_iteration==1)
      closure_accumulated <-  recommendation$closure$get_vector()
    else
       closure_accumulated <- cbind(closure_accumulated,recommendation$closure$get_vector())

 #   S <- sparse_set$new(attributes = colnames(FC),M=fcaR:::.multiunion(closure_accumulated))

    S <- SparseSet$new(attributes = colnames(FC))
#    browser()
	S$assign(attributes = colnames(FC)[as.logical(fcaR:::.multiunion(closure_accumulated))],values = 1)

    objects_global <-  intent(FC,S )

    # SAVING IN DATA.FRAME AGREGATIONS
    dt_closure1 <- data.frame(execution=num_execution,
                                   experiment=num_experiment,
                                   iteration=n_iteration,
                                   closure=c(colnames(FC)[as.logical(S$get_vector())]))
    dt_closure <- rbind(dt_closure,dt_closure1)
    dt_agregation_pref <- agregation2df(fcaR_fc$attributes,
                                        num_execution,
                                        num_experiment,
                                        n_iteration,
                                        recommendation$next_attributes,
                                        recommendation$closure,
                                        dt_agregation_pref,
                                        dt_experiment_pref)

    fcaR_fc <- recommendation$new_fcaR_fc

#    if (is.null(next_attributes) | is.null(fcaR_fc)  ){
#          cat(">>> FIN:\n")
#    }
#    else{
#    if (!no_end_condition_experiment(next_attributes,
    #                                  fcaR_fc,
    #                                  objects_global)){
    #   cat(">>> FIN1:\n")
    #   return(list("recommendation_global"=objects_global,
    #               "pref_users"=dt_experiment_pref,
    #               "pref_agregated"=dt_agregation_pref,
    #               "pref_sizes"=dt_experiment_sizes,
    #               "last_iteration_recommendation"=recommendation,
    #               "final_closure"=dt_closure))
    # }#end if

    if (verbose)
      display_recommendation(objects_global,
                           fcaR_fc,
                           recommendation)
}#end while

return(list("recommendation_global"=objects_global,
            "pref_users"=dt_experiment_pref,
            "pref_agregated"=dt_agregation_pref,
            "pref_sizes"=dt_experiment_sizes,
            "last_iteration_recommendation"=recommendation,
            "final_closure"=dt_closure))}



no_end_condition_experiment <- function(next_attributes,fcaR_fc,objects_global){
#  browser()


  if  (is.null(fcaR_fc)) {
    cat("_________________ End 1: _______\n")
    cat(">>> FCA null\n")
    return(FALSE)
  }
  else if (is.null(next_attributes)){
    cat("_________________ End 2: _______\n")
    cat(">>> Next Attributes null\n")
    return(FALSE)
  }
  else if (length(objects_global)<2){
    cat("_________________ End 3: _______\n")
    cat(">>>   Objects less than 2\n")
    return(FALSE)
  }
  if (length(next_attributes)==0){
    cat("_________________ End 4: _______\n")
    cat(">>> Next Attributes <=0\n")
    return(FALSE)
  }
  if ( fcaR_fc$implications$cardinality()==0){
    cat("_________________ End 5: _______\n")
    cat(">>> Number implications is 0\n")
    return(FALSE)
  }
  return(TRUE)
}

display_recommendation <- function(objects_global,
                                   fcaR_fc,
                                   recommendation
                                   ){
    cat("_________________ Recommendation is: _______\n")
    cat(">>> Objects in the recommendation global:\n")
    cat(objects_global)
    cat("\n")
    cat(">>> Closure in this iteration: \n")
    my_closure <-  fcaR_fc$attributes[as.logical(recommendation$closure$get_vector())]

    cat(my_closure)
    cat("\n")
    cat("\n")
    cat(">>> Next attributes to explore in next iteration :\n")
    next_attributes <- recommendation$next_attributes
    cat(next_attributes)
    cat("\n")
    cat(">>> Rules in next iteration: \n")
    if (!is.null(fcaR_fc))
        print(fcaR_fc$implications$cardinality())
    else cat("FC NULL - no implications \n")
    cat("\n")
}#end function


# auxiliars
initialize.df <- function(){
  dt_closure <- data.frame(execution=c(),
                           experiment=c(),
                           iteration=c(),
                           closure=c())
  dt_experiment_pref <- data.frame(execution=c(),
                                   experiment=c(),
                                   iteration=c(),
                                   user=c(),
                                   attribute=c(),
                                   grade=c())
  dt_agregation_pref <- data.frame(execution=c(),
                                   experiment=c(),
                                   iteration=c(),
                                   attribute=c(),
                                   grade=c(),
                                   InClosure=c())
  dt_experiment_sizes <- data.frame(execution=c(),
                                    experiment=c(),
                                    iteration=c(),
                                    sigma=c(),
                                    #closure=c(),
                                    attributes=c(),
                                    objects=c(),
                                    closure=c())
  return(list("closure"=dt_closure,
              "pref"=dt_experiment_pref,
              "agregation_preg"=dt_agregation_pref,
              "sizes"=  dt_experiment_sizes

  ))
}#function
