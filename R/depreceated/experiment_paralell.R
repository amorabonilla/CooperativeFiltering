# noparalell_experiment_recommendation <-function(NumberOfCluster,
#                                                num_execution,
#                                                nombre_experiment,
#                                                num_objects_ini,num_objects_fin,
#                                                num_attributes_ini,
#                                                num_attributes_fin,
#                                                spar_degree,
#                                                n_users,num_scales,my_support){
#   
#   
#   
#   name_dt_experiment_pref <-stringr::str_c("output/",
#                                            nombre_experiment,"_pref_users.csv")
#   name_dt_agregation_pref <-stringr::str_c("output/",
#                                            nombre_experiment,"_pref_agregated.csv")
#   name_dt_experiment_sizes <-stringr::str_c("output/",
#                                             nombre_experiment,"_pref_sizes.csv")
#   name_dt_experiment_size_initial <-stringr::str_c("output/",
#                                                    nombre_experiment,"_experiment_sizes.csv")
#   
#   name_dt_closure <-stringr::str_c("output/",
#                                    nombre_experiment,"_closure.csv")
#   
#   
#   #initialize.df
#   dt <- initialize.df()
#   dt_closure  <- dt$closure
#   dt_experiment_pref <- dt$pref
#   dt_agregation_pref <- dt$agregation_preg
#   dt_experiment_sizes <- dt$sizes
#   
#   
#   dt_experiment_size_initial <- data.frame(execution=c(),
#                                            experiment=c(),
#                                            num_attributes=c(),
#                                            num_objects=c()) 
#   
#   
#   num_experiment <- 1
#   
#   
#   cl <- makeCluster(NumberOfCluster)
#   registerDoSNOW(cl)
#   
#   
#   for (obj in seq(num_objects_ini,num_objects_fin,5))
#     foreach::foreach(att=seq(num_attributes_ini,num_attributes_fin,5)
#                      ,.combine = "rbind", 
#                      .packages=c('CooperativeFiltering', 'arules',
#                                  'magrittr','fcaR',
#                                  'tidyverse','sets')) %dopar%
#     {
#       #    browser()
#       cat("\n")
#       cat("----------------------------------------")
#       cat("Experiment: ",num_experiment,"\n")
#       cat("  Objects: ",obj,"\n")
#       cat("  Attributes: ",att,"\n")
#       
#       #  browser()
#       t <-system.time(pref <- random_experiment(num_execution,
#                                                 num_experiment,
#                                                 obj,
#                                                 att,
#                                                 spar_degree,
#                                                 n_users,
#                                                 num_scales,
#                                                 my_support,
#                                                 verbose=TRUE))
#       
#       #    browser()
#       dt_experiment_size_initial1 <- data.frame(execution=c(num_execution),
#                                                 experiment=c(num_experiment),
#                                                 num_attributes=c(att),
#                                                 num_objects=c(obj)) 
#       
#       cat("Tiempo\n")
#       cat(t)
#       cat("Preferencias\n")
#       pref
#       #      browser()
#       if (num_experiment==1){ 
#         dt_experiment_pref <- rbind(dt_experiment_pref,
#                                     pref$pref_users)
#         
#         
#         dt_agregation_pref <- rbind(dt_agregation_pref,
#                                     pref$pref_agregated)
#         #        dt_agregation_pref[dt_agregation_pref =="NULL"] <- NA
#         dt_agregation_pref$grade <- mean(unlist(dt_agregation_pref$grade))
#         #  por aqui hacer la media y ejecutar
#         
#         dt_experiment_sizes <- rbind(dt_experiment_sizes,
#                                      pref$pref_sizes)
#         dt_experiment_size_initial <- rbind(dt_experiment_size_initial,
#                                             dt_experiment_size_initial1)
#         
#         dt_closure <- rbind(dt_closure,pref$final_closure)
#         
#         
#         write.csv(dt_experiment_pref,
#                   name_dt_experiment_pref, row.names = FALSE)
#         write.csv(dt_agregation_pref,
#                   name_dt_agregation_pref, row.names = FALSE)
#         
#         write.csv(dt_experiment_sizes,
#                   name_dt_experiment_sizes, row.names = FALSE)
#         write.csv(dt_experiment_size_initial,
#                   name_dt_experiment_size_initial, row.names = FALSE)
#         
#         write.csv(dt_closure,
#                   name_dt_closure, row.names = FALSE)
#         
#       } else {
#         #       browser()
#         dt_experiment_pref <-read.csv(name_dt_experiment_pref)
#         dt_agregation_pref <- read.csv(name_dt_agregation_pref)
#         dt_experiment_sizes <- read.csv(name_dt_experiment_sizes)
#         dt_experiment_size_initial <- read.csv(name_dt_experiment_size_initial)
#         dt_closure <- read.csv(name_dt_closure)
#         
#         dt_experiment_pref <- rbind(dt_experiment_pref,
#                                     pref$pref_users)
#         dt_agregation_pref <- rbind(dt_agregation_pref,
#                                     pref$pref_agregated)
#         
#         dt_agregation_pref$grade <- mean(unlist(dt_agregation_pref$grade))
#         
#         dt_experiment_sizes <- rbind(dt_experiment_sizes,
#                                      pref$pref_sizes)
#         dt_experiment_size_initial <- rbind(dt_experiment_size_initial,
#                                             dt_experiment_size_initial1)
#         
#         dt_closure <- rbind(dt_closure,pref$final_closure)
#         
#         write.csv(dt_experiment_pref,
#                   name_dt_experiment_pref, row.names = FALSE)
#         write.csv(dt_agregation_pref,
#                   name_dt_agregation_pref, row.names = FALSE)
#         
#         write.csv(dt_experiment_sizes,
#                   name_dt_experiment_sizes, row.names = FALSE)
#         write.csv(dt_experiment_size_initial,
#                   name_dt_experiment_size_initial, row.names = FALSE)
#         
#         write.csv(dt_closure,
#                   name_dt_closure, row.names = FALSE)
#         
#       }
#       
#       
#       num_experiment <- num_experiment+1
#       #
#       
#       
#     }#end for
#   
#    
#   stopCluster(cl) 
# 
# 
# 
# }#end function
 
paralell_experiment_recommendation <-function(NumberOfCluster,
                                              num_execution,
                              nombre_experiment,
                              num_objects_ini,num_objects_fin,
                              num_attributes_ini,
                              num_attributes_fin,
                              spar_degree,
                              n_users,num_scales,my_support){

  name_dt_experiment_pref <-stringr::str_c("output/",
                                           nombre_experiment,"_pref_users.csv")
  name_dt_agregation_pref <-stringr::str_c("output/",
                                           nombre_experiment,"_pref_agregated.csv")
  name_dt_experiment_sizes <-stringr::str_c("output/",
                                            nombre_experiment,"_pref_sizes.csv")
  name_dt_experiment_size_initial <-stringr::str_c("output/",
                                                   nombre_experiment,"_experiment_sizes.csv")
  
  name_dt_closure <-stringr::str_c("output/",
                                   nombre_experiment,"_closure.csv")
  
  
  #initialize.df
  dt <- initialize.df()
  dt_closure  <- dt$closure
  dt_experiment_pref <- dt$pref
  dt_agregation_pref <- dt$agregation_preg
  dt_experiment_sizes <- dt$sizes
  
  
  dt_experiment_size_initial <- data.frame(execution=c(),
                                           experiment=c(),
                                           num_attributes=c(),
                                           num_objects=c()) 
  
  
  num_experiment <- 1
  
  
  
  
  cl <- makeCluster(NumberOfCluster)
  registerDoSNOW(cl)
  
  
  for (obj in seq(num_objects_ini,num_objects_fin,5))
    foreach::foreach(att=seq(num_attributes_ini,num_attributes_fin,5)
                      ,.combine = rbind, 
                     .packages=c('CooperativeFiltering', 'arules',
                                 'magrittr','fcaR',
                                 'tidyverse','sets')) %dopar%
    {
                      #.combine=cbind, 
                      #.packages='raster') %dopar%
      
      cat("\n")
      cat("----------------------------------------")
      cat("Experiment: ",num_experiment,"\n")
      cat("  Objects: ",obj,"\n")
      cat("  Attributes: ",att,"\n")
      
     
      t <-system.time(pref <- random_experiment(num_execution,
                                                num_experiment,
                                                obj,
                                                att,
                                                spar_degree,
                                                n_users,
                                                num_scales,
                                                my_support,
                                                verbose=TRUE))
      num_experiment <- num_experiment+1
      
    }
}
#browser()
      dt_experiment_size_initial1 <- data.frame(execution=c(num_execution),
                                                experiment=c(num_experiment),
                                                num_attributes=c(att),
                                                num_objects=c(obj)) 
      
      cat("Tiempo\n")
      cat(t)
      cat("Preferencias\n")
      pref
      #      browser()
      if (num_experiment==1){ 
        dt_experiment_pref <- rbind(dt_experiment_pref,
                                    pref$pref_users)
        
        
        dt_agregation_pref <- rbind(dt_agregation_pref,
                                    pref$pref_agregated)
        #        dt_agregation_pref[dt_agregation_pref =="NULL"] <- NA
        dt_agregation_pref$grade <- mean(unlist(dt_agregation_pref$grade))
        #  por aqui hacer la media y ejecutar
        
        dt_experiment_sizes <- rbind(dt_experiment_sizes,
                                     pref$pref_sizes)
        dt_experiment_size_initial <- rbind(dt_experiment_size_initial,
                                            dt_experiment_size_initial1)
        
        dt_closure <- rbind(dt_closure,pref$final_closure)
        
        
        write.csv(dt_experiment_pref,
                  name_dt_experiment_pref, row.names = FALSE)
        write.csv(dt_agregation_pref,
                  name_dt_agregation_pref, row.names = FALSE)
        
        write.csv(dt_experiment_sizes,
                  name_dt_experiment_sizes, row.names = FALSE)
        write.csv(dt_experiment_size_initial,
                  name_dt_experiment_size_initial, row.names = FALSE)
        
        write.csv(dt_closure,
                  name_dt_closure, row.names = FALSE)
        
      } else {
        #       browser()
        dt_experiment_pref <-read.csv(name_dt_experiment_pref)
        dt_agregation_pref <- read.csv(name_dt_agregation_pref)
        dt_experiment_sizes <- read.csv(name_dt_experiment_sizes)
        dt_experiment_size_initial <- read.csv(name_dt_experiment_size_initial)
        dt_closure <- read.csv(name_dt_closure)
        
        dt_experiment_pref <- rbind(dt_experiment_pref,
                                    pref$pref_users)
        dt_agregation_pref <- rbind(dt_agregation_pref,
                                    pref$pref_agregated)
        
        dt_agregation_pref$grade <- mean(unlist(dt_agregation_pref$grade))
        
        dt_experiment_sizes <- rbind(dt_experiment_sizes,
                                     pref$pref_sizes)
        dt_experiment_size_initial <- rbind(dt_experiment_size_initial,
                                            dt_experiment_size_initial1)
        
        dt_closure <- rbind(dt_closure,pref$final_closure)
        
        write.csv(dt_experiment_pref,
                  name_dt_experiment_pref, row.names = FALSE)
        write.csv(dt_agregation_pref,
                  name_dt_agregation_pref, row.names = FALSE)
        
        write.csv(dt_experiment_sizes,
                  name_dt_experiment_sizes, row.names = FALSE)
        write.csv(dt_experiment_size_initial,
                  name_dt_experiment_size_initial, row.names = FALSE)
        
        write.csv(dt_closure,
                  name_dt_closure, row.names = FALSE)
        
      
      
      
      num_experiment <- num_experiment+1
      #
      
      
   # }#end for
  
  
  
  
  #  browser()
  #  dt_experiment_pref <- sapply(dt_experiment_pref,unlist)
  #   dt_agregation_pref <- sapply(dt_agregation_pref,unlist)
  
  
  #  dt_experiment_sizes <- sapply(dt_experiment_sizes,unlist)
  
stopCluster(cl)
}


 
 