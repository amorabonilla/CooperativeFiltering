 
itemmatrix.to.vector <- function(A){
  A1 <- as(A,"matrix")
  colnames(A1)[which(A1)]
}

vector.to.sparse <- function(Attributes){
  iLabels <- sort(Attributes)
  return(encode(Attributes, iLabels))
}

# vector to the representation of attributes of fcaR
to.sparse <- function(Attributes, attributes){
  n <- length(Attributes)
  S <- build_set(attrs = Attributes,
                 values = rep(1,n),attributes)
  return(S )
}

from.sparse <- function(Att1, attributes){
  attributes[which(Att1==1)]
}#end function


set.to.sparse <- function(X, ListAttributes){
  X <- sort(X)
  p <- length(ListAttributes)
  names(p) <- NULL
  p <- cumsum(p)
  i <- unlist(ListAttributes, use.names = FALSE)
  i <- factor(i)
  p <- new("ngCMatrix", p   = c(0L, p),
           i   = c(i) - 1L,
           Dim = c(length(levels(i)), length(p)))
  
  X.sparse <- new("itemMatrix", 
      data        = p,
      itemInfo    = data.frame(labels    = levels(i), 
                               stringsAsFactors = FALSE),
      itemsetInfo = data.frame(itemsetID = labels(ListAttributes), 
                               stringsAsFactors = FALSE))
  return(encode(list(X), sort(ListAttributes)))
}#End initialize.setOfAttributes


empty.itemmatrix <- function(my_attributes){
  A <-initialize.setOfAttributes(my_attributes[1],my_attributes)
  return(arules::itemSetdiff(A,A))
}

initialize.setOfAttributes <- function(X, ListAttributes){
  X <- unique(X)
  if(last(ListAttributes)=="empty"){
    l <- length(ListAttributes)
    iLabels <- (ListAttributes[-l])  #quitado sort
    iLabels <- c(iLabels, "empty")
  }else{
    iLabels <- (ListAttributes)
  }
  X2 <- encode(list(X), iLabels)
  return(X2)
}#End initialize.setOfAttributes



set.of.attributes <- function(ListAttributes){
  list <- c()
  for (k in seq(ListAttributes)){
    imp <- strsplit(str_trim(ListAttributes[k],side=c("both"))," ")
    list <- union(imp,list)
  }
  return(list)
}#End set.of.attributes
  


is.singleton <- function(X){
  return(length(X)==1)
}#End is.singleton



union.sets <- function(X,Y){
  arules::itemUnion(X,Y)  
}#End union.sets



intersection.sets <- function(X,Y){
  arules::itemIntersect(X,Y)  
}#End intersection.sets



difference.sets <- function(X,Y){
  arules::itemSetdiff(X,Y)
}#End difference.sets



is.included <- function(X,Y){
  return(arules::is.subset(X, Y, proper=FALSE, sparse=FALSE))
}#End is.included

is.included.proper <- function(X,Y){
  return(arules::is.subset(X, Y, proper=TRUE, sparse=FALSE))
}#End is.included



is.empty <- function(X){
  return(size(X)==0)
}#End is.empty
  


is.empty.set <- function(X){
  return(length(X[[1]])==0)  
}#End is.empty.set



equals.sets <- function(X,Y){
  return(arules::setequal(X,Y))  
}#End equals.sets


set.lower.i <- function(Attributes,A,i){
  
}

##################################
#  MANEJAR IMPLICACIONES INDIVIDUALES
##################################
empty_Sigma <- function(FC){
  iLabels <- colnames(FC)
  iM <- encode(list(), iLabels) 
  rNJ <- new("rules", lhs=iM, rhs=iM)
}

empty_left_rule <- function(FC,A){
  iLabels <- colnames(FC)
  left <- encode(list(), iLabels) 
  right <- encode(list(A),iLabels)
  rNJ <- new("rules", lhs=right[0], rhs=right)
}


remove.imp2 <- function(Sigma, k){
  rNJ <- new("rules", lhs=lhs(Sigma[k]), rhs=itemSetdiff(lhs(Sigma[k]), lhs(Sigma[k])), quality = data.frame(confidence = 1))
  return(c(Sigma[seq_len(k-1)], rNJ, Sigma[k+seq_len(length(Sigma)-k)]))
}#End remove.imp2



remove.imp <- function(Sigma, k){
  return(Sigma[-k])
}#End remove.imp



remove.all <- function(Sigma){
  return(Sigma[0])
}#End remove.all



add.imp.k <- function(Sigma,A,B,k){
  rNJ <- new("rules", lhs=A, rhs=B, quality = data.frame(confidence = 1))
  SigmaNew <- c(Sigma[seq_len(k-1)], rNJ, Sigma[k+seq_len(length(Sigma)-k)])
  return(SigmaNew)
}#End add.imp.k



add.imp <- function(Sigma,A,B){
  rNJ <- new("rules", lhs=A, rhs=B,quality = data.frame(confidence = 1))
  Sigma <- c(Sigma,rNJ)
  return(Sigma)
}#End add.imp



new.imp <- function(Sigma,A,B){
  
  for (k in seq(Sigma)){
    C <- read.left(Sigma,k)
    D <- read.right(Sigma,k) 
    if (equals.sets(C,A)){
      if (equals.sets(D,B)){
        return(Sigma) # ya est?
      }else{
        BD <- union.sets(B,D) #union of B with D (other with A in the premise)
        Sigma <- add.imp.k(Sigma,A,BD, k)
        return(Sigma)  
      }#end else
    }#end if
  }#end for
  Sigma <- add.imp(Sigma,A,B)
  return(Sigma)  
}#End new.imp



############################   AUXILIARS

read.left <- function(Sigma,k) {
   return(lhs(Sigma[k]))
 }#End read.left 



read.right <- function(Sigma,k) {
  return(rhs(Sigma[k]))
}#End read.right



substitute.imp <- function(Sigma, k, newC, newD){
  Sigma <- add.imp.k(Sigma, newC, difference.sets(newD,newC), k)
  return(Sigma)
}#End substitute.imp



included.left <- function(Sigma, k, l){
  return(is.included(read.left(Sigma,k), read.left(Sigma,l)))
}#End included.left



included.left.right <- function(Sigma,k, l){
  uni1 <- union.sets(read.reft(Sigma,l), read.right(Sigma,l))
  return(is.included(read.left(Sigma,k), uni1))
}#End included.left.right



delete.set.of.IS <- function(Sigma){
  k <- seq(Sigma)  
  Index <- !is.empty(read.right(Sigma,k))
  return(Sigma[Index])
}#End delete.set.of.IS



####################################### NEW FUNCTIONS

size.set <- function(Sigma){
  return(sum(size(Sigma)))
}#End size.set



cardinality.set <- function(Sigma){
  return(length(Sigma))
}#End cardinality.set



####################################### minimal key reduction

core <- function(Omega, Gamma){
  rpSet <- read.right(Gamma,1)
  for(k in seq(Gamma)){
    B <- read.right(Gamma,k)
    rpSet <- union.sets(rpSet,B)
  }
  newOmega <- difference.sets(Omega, rpSet)
  return(newOmega)
}#End core



body <- function(Omega, Gamma){
  lpSet <- read.left(Gamma,1)
  for(k in seq(Gamma)){
    B <- read.left(Gamma,k)
    lpSet <- union.sets(lpSet,B)
  }
  OmegaC <- core(Omega,Gamma)
  OmegaClos <- apply.closure(Gamma,OmegaC)
  newSet <- difference.sets(lpSet, OmegaClos$closure)
  return(newSet)
}#End body
