
random_experiment_dataset <- function(Rules, FC,name_file_rules,n_users,num_scales,my_support,num_execution,
                                      num_experiment,verbose ){
   #  browser()
  # to save results and initialization
  dt <- initialize.df()
  dt_closure  <- dt$closure
  dt_experiment_pref <- dt$pref
  dt_agregation_pref <- dt$agregation_preg
  dt_experiment_sizes <- dt$sizes
  closure_accumulated <- c()
  n_iteration <- 0
  #  browser()

  # rules to formal context
#  Rules <- read.PMML(name_file_rules)
  fcaR_fc <- FormalContext$new(FC)
  fcaR_fc$implications$add(Rules)
 # fcaR_fc$implications

  objects_global=rownames(FC)

  num_exp <- 1
#  Attributes <- fcaR_fc$attributes
#  Rules <- fcaR_fc$rules
  next_attributes <- fcaR_fc$attributes

  dt_experiment1_sizes <- data.frame(execution=num_execution,
                                     experiment=num_experiment,
                                     iteration=c(n_iteration),
                                     sigma=c(fcaR_fc$implications$cardinality()),
                                     attributes=c(length(next_attributes)),
                                     objects=length(objects_global),
                                     closure=0)
  dt_experiment_sizes <- rbind(dt_experiment_sizes,dt_experiment1_sizes)


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


      #    browser()    # END SAVING
      recommendation <- random_filtering_features(FC,fcaR_fc,#name_file_rules,
                                                  next_attributes,
                                                  #Rules,
                                                  #list_users,
                                                  dt_preferences)
      #    if (is.null(recommendation$new_fcaR_fc))
      #        browser()
      #    if (is.null(fcaR_fc)) {
      # SAVING IN DATA.FRAME SIZES
      # browser()


 #     browser()
      if (n_iteration==1)
        closure_accumulated <-  recommendation$closure$get_vector()
      else
        closure_accumulated <- cbind(closure_accumulated,recommendation$closure$get_vector())

      #   S <- sparse_set$new(attributes = colnames(FC),M=fcaR:::.multiunion(closure_accumulated))

      S <- SparseSet$new(attributes = colnames(FC))
      #    browser()
      # pruebo a comentar esto
      attr_closure_accumulated <-  colnames(FC)[as.logical(fcaR:::.multiunion(closure_accumulated))]
      S$assign(attributes = attr_closure_accumulated,values = 1)

      objects_global <-  intent(FC,S )

 #     browser()
      dt_experiment1_sizes <- data.frame(execution=num_execution,
                                         experiment=num_experiment,
                                         iteration=c(n_iteration),
                                         sigma=c(fcaR_fc$implications$cardinality()),
                                         attributes=c(length(recommendation$evaluated_attributes)),
                                         objects=length(objects_global),
                                         closure=length(attr_closure_accumulated))

      dt_experiment_sizes <- rbind(dt_experiment_sizes,dt_experiment1_sizes)

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
#      browser()
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
