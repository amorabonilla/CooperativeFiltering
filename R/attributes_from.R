# collect attributes from minimals
attributes_from_minimals <- function(M_minimals){
#   browser()
   M <- as(M_minimals,"matrix")
#   sum_M <- colSums(M)
   sum_M <- rowSums(M)
   return(names(which(sum_M!=0)))
}# end function




# collect attributes form rules

attributes_from_lhs <-function(Sigma){
   my_set <- lhs(Sigma[1])
   for (k in seq_len(length(Sigma))){
      my_set <- union.sets(my_set,lhs(Sigma[k]))
   }
   return(my_set)
}# end function


attributes_from_rules <-function(Sigma){
   my_set <- lhs(Sigma[1])
   for (k in seq_len(length(Sigma))){
     my_set <- union.sets(my_set,lhs(Sigma[k]))
     my_set <- union.sets(my_set,rhs(Sigma[k]))
   }
   return(my_set)
}# end function


attributes_from_Next <- function(Next){
   n <- sample(seq_len(length(Next)),1)
   return(Next[n])
}

attributes_from_table <- function(attributes){
#  n <- sample(seq_len(dim(attributes)[2]),1)
  n <- length(attributes)
  if (n==1) return( attributes)
  if (n>10){
      n1 <- 2
      # 1 elecciones como maximo
      nrandom <- base::sample(1:n1,1)
   } else {nrandom <- base::sample(1:2,1)}
  values <- base::sample(1:n,nrandom)
  return( attributes[values])
}

random_grades <- function(n,num_scales){
   #  n <- sample(seq_len(dim(attributes)[2]),1)
      nrandom <- base::sample(seq(1/num_scales,1,by=1/num_scales),n,replace = TRUE)
   return( nrandom)
}

#old


attributes_from_minimals2 <- function(minimals){
   browser()
   my_set <- minimals[1]
   for (k in seq_len(length(minimals))){
      my_set <- union.sets(my_set,minimals[k])
   }

   return(my_set)
}# end function
