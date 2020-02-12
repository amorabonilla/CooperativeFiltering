 

agregation1 <- function(Att,dt_attributes){
  for (l in seq_len(length(Att))){
    dt_attributes$agregated_degree[[l]] <- 
      mean(unlist(dt_attributes$agregated_degree[[l]]),na.rm = TRUE)
  }#end for  
  return(dt_attributes)
}#end function

agregation2 <- function(IS, Att,dt_attributes){
#cat("***  dt.attrib ***\n")
#print(dt_attributes)
#  Rules <- IS
  #  browser()
  if (IS$cardinality()==1){
    lhs_minimal_sets <- IS$get_LHS_matrix()  
    num_lhs_minimal_sets <- 1}
  else{
     lhs_minimal_sets  <- lhs.minimals(IS)
     num_lhs_minimal_sets <- lhs_minimal_sets@Dim[2]
     
  }#end else   
  dt_min_lhs <- data.frame(num_min_lhs=1:num_lhs_minimal_sets)  
  dt_min_lhs <- 
    dt_min_lhs %>%
    mutate(degree_min_lhs =sapply(dt_min_lhs$num_min_lhs,
                               function(x) { 
                                 ind <-which(as.logical(lhs_minimal_sets[,x]))
                                 ind_dt_att <-sapply(ind,function(x)
                                        which(dt_attributes$attribute == Att[x])
                                   )#end sapply
                                 if (is.list(ind_dt_att)) ind_dt_att <- unlist(ind_dt_att)
                                 
#                                 ind_dt_att <- which(dt_attributes$attribute == Att[ind])
                                 mean(unlist(dt_attributes$agregated_degree[ind_dt_att]),na.rm = TRUE)
#                                 mean(unlist(dt_attributes$agregated_degree[lhs_minimal_sets[x]@data@i+1]),na.rm = TRUE)
                                 })) %>%
    arrange(desc(degree_min_lhs))

#  print(dt_min_lhs browser()
  dt_min_lhs <- head(dt_min_lhs,4)
   
#  browser()
  if (num_lhs_minimal_sets==1){

#    S <- sparse_set$new(attributes =Att,
#                        c(lhs_minimal_sets,lhs_minimal_sets))

    S <- SparseSet$new(attributes = Att)
	S$assign(attributes = c(lhs_minimal_sets,lhs_minimal_sets), values = 1)
    closure_lhs_minimal_sets <- list(IS$closure(S,reduce = TRUE,verbose=FALSE))
  }
  else{
    closure_lhs_minimal_sets <- lapply(dt_min_lhs$num_min_lhs,
                                      function(x){
                                        #S <- sparse_set$new(attributes =Att,
                                        #                    lhs_minimal_sets[,x])
    									S <- SparseSet$new(attributes = Att)
										S$assign(attributes = 
										           Att[as.logical(lhs_minimal_sets[,x])], 
											values = 1)
                                        IS$closure(S,reduce = TRUE,verbose=FALSE)
 
                                      })
  }#end else  
   
 
    
    n_i <- length(IS$get_attributes())
    Attributes <- IS$get_attributes()
    dt_attributes_ampliada <-  update_dt_attributes(n_i, Attributes,dt_attributes)
  # POR AQUI USAR dt_attributes_ampliada OCN CLOSURE
    
    dt_min_closure_lhs  <-data.frame(num_min_closure_lhs=
                                     1:(length(closure_lhs_minimal_sets))) 
    
  dt_min_closure_lhs <- 
    dt_min_closure_lhs %>%
    mutate(degree_min_closure_lhs =
             sapply(dt_min_closure_lhs$num_min_closure_lhs,
                    function(x) { 
                      Clos_i <- closure_lhs_minimal_sets[[x]]$closure
                      mean(as.matrix(Clos_i$get_vector())*
                           unlist(dt_attributes_ampliada$agregated_degree))
                                  }))   %>%
    arrange(desc(degree_min_closure_lhs))
#    top_n(1)

#    print(dt_min_closure_lhs)
#  browser()
  dt_min_closure_lhs <- head(dt_min_closure_lhs,1)
   return(list("closure"= closure_lhs_minimal_sets[[dt_min_closure_lhs$num_min_closure_lhs]]$closure,
          "Rules"=closure_lhs_minimal_sets[[dt_min_closure_lhs$num_min_closure_lhs]]$implications))
#  return(list("closure"=Att[as.logical(closure_lhs_minimal_sets[[1]]$closure$get_vector())],
#              "Rules"=closure_lhs_minimal_sets[[1]]$implications$to_arules()))
}#end function


update_dt_attributes <- function( n_i, Attributes,dt_attributes ){
  dt_attributes_ampliada <-  data.frame(attributes=Attributes, agregated_degree=rep(0, n_i))
  for (x in dt_attributes$attributes){
    ind <- which(dt_attributes_ampliada$attributes == x)
    ind2 <-  which(dt_attributes$attributes == x)
    dt_attributes_ampliada$agregated_degree[ind] <- dt_attributes$agregated_degree[ind2]
  }
  
  
  dt_attributes_ampliada <- dt_attributes_ampliada %>% replace(.=="NULL", 0) 
  
  return(dt_attributes_ampliada)
}#end function
  
  
  
