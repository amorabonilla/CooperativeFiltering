
next.min.gen <- function(X,Sigma){
  fixpoint <- FALSE
  while (!fixpoint){
    n <- length(Sigma)
    if (n== 0) return(list("closure"=X, "Next"=NULL,"implications"=NULL))
    Gamma <- Sigma;Sigma <- Gamma[0]
    fixpoint <- TRUE
    lhs_included <- is.included(lhs(Gamma),X)
    indices_lhs_included <- which(lhs_included)
    if (length(indices_lhs_included)==0) indices_no_lhs_included  <- 1:n
    else  indices_no_lhs_included<- which(!lhs_included)
# browser()
    if (length(indices_lhs_included)>0){
        for (k in  indices_lhs_included){
            B <- rhs(Gamma[k])
            X <-  union.sets(X,B)
            fixpoint <- FALSE}
    }
    if (length(indices_no_lhs_included)>0){
      for (k in  indices_no_lhs_included){
#        browser()
            A <- lhs(Gamma[k])
            B <- rhs(Gamma[k])
#            if (!is.included(B,X)|!is.included(A,X)){
            if (!is.included(B,X)){
                A_X <- difference.sets(A,X)
                B_X <- difference.sets(B,X)
                if (!equals.sets(A_X,A)|!equals.sets(B_X,B)){
                      fixpoint <- FALSE
                Sigma <- add.imp(Sigma,A_X,B_X)
                } else {   Sigma <- add.imp(Sigma,A,B)}#end if
            }#ebd if
      }#end for
    }#end if

  }#end while
  Next <- lhs.minimals(Sigma)

   # inspect(Next);inspect(X);inspect(Gamma)

  return(list("closure"=X, "Next"=Next,"implications"=Sigma))
}#End NextMinGen





lhs.minimals <- function(IS){
  # browser()
#  IS <- implication_set$new()
#  IS$from_arules(Sigma)
#  IS$apply_rules(rules = c("composition","simplification"))
  LHS_matrix <- IS$get_LHS_matrix()
  M <- fcaR:::.subset(LHS_matrix)
  ind <- which(Matrix::colSums(M)==1)

  LHS_matrix_minimals <- IS$get_LHS_matrix()[,ind]
  return(LHS_matrix_minimals)
}#end function



next.min.gen.old <- function(X,Sigma){
  fixpoint <- FALSE
  while (!fixpoint){
    if (length(Sigma)== 0) return(list("closure"=X, "Next"=NULL,"implications"=NULL))
    Gamma <- Sigma
    Sigma <- Gamma[0]
    fixpoint <- TRUE
    for (k in seq_len(length(Gamma))){
      A <- lhs(Gamma[k])
      B <- rhs(Gamma[k])
      if (is.included(A,X)){
        X <- union.sets(X,B)}
      else {
        if (!is.included(B,X)){
          A_X <- difference.sets(A,X)
          B_X <-difference.sets(B,X)
          if (!equals.sets(A_X,A)|!equals.sets(B_X,B)){
            Sigma <- add.imp(Sigma,A_X,B_X)
            fixpoint <- FALSE
          }#end if
          else   Sigma <- add.imp(Sigma,A,B)
        }#end if
      }# end if
    }#end for
  }#end while
  Next <- Minimals(Sigma)
  return(list("closure"=X, "Next"=Next,"implications"=Sigma))
}#End NextMinGen

