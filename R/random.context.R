
label_att <- function(n) paste0('att',n)
label_obj <- function(n) paste0('obj',n)




rbv <- function(n) sample(0:1,n,replace = T)



context.no.sparness <- function(num_obj,num_attr) {
  mi_df <- data.frame(rbv(num_obj))
  for (k in 1:(num_attr-1)){
      col <- rbv(num_obj)
      mi_df <- cbind(mi_df,col)
  }
  return(mi_df)
}#End context.no.sparness

 
context <- function(num_obj, num_attr, sparness=NULL, namefile="context") {
  if (num_obj < 1 || num_attr < 1){
    stop("The number of objects and the number of attributes must be greater than zero.")
  } 
  if (!is.null(sparness) && ((sparness < 0) || (sparness > 1))){
    stop("Sparness must be a number between 0 and 1.") 
  }
  if(is.null(sparness)){
    mi_df <- context.no.sparness(num_obj, num_attr)
  }else{
    totalN <- num_obj*num_attr
    ones <- totalN*sparness
    if(ones < totalN/2){
      mi_df <- putOnes(mi_df, totalN, ones, num_obj, num_attr)
    }else{
      mi_df <- putZeros(mi_df, totalN, (totalN-ones), num_obj, num_attr)
    }
  }
  colnames(mi_df) <- as.vector(sapply(1:num_attr,label_att))
  rownames(mi_df) <- as.vector(sapply(1:num_obj,label_obj))
  
  # for(k in seq(dim(mi_df)[2])){
  #   mi_df[,k] <- as.logical(mi_df[,k])
  # }
  namefile <- paste(namefile, ".csv", sep="")
  write.csv(mi_df, file=namefile)
  
  return(mi_df)
}#End context



putOnes <- function(mi_df, totalN, ones, num_obj, num_attr){
  bin <- rep(0, totalN)
  contI <- 1
  contF <- num_obj
  mi_df <- data.frame(bin[contI:contF])
  for (k in seq(num_attr-1)){  
    contI <- contI+num_obj
    contF <- contF+num_obj
    col <- bin[contI:contF]
    mi_df <- cbind(mi_df,col)
  }
  numRow <- 0
  numCol <- 0
  sum <- 0
  while(sum < ones){
    numRow <- sample(1:num_obj,1)
    numCol <- sample(1:num_attr,1)
    if(mi_df[numRow,numCol] == 0){
      mi_df[numRow,numCol] <- 1
      sum <- sum + 1
    }
  }
  return (mi_df)
}#End putOnes



putZeros <- function(mi_df, totalN, zeros, num_obj, num_attr){
  bin <- rep(1, totalN)
  contI <- 1
  contF <- num_obj
  mi_df <- data.frame(bin[contI:contF])
  for (k in seq(num_attr-1)){  
    contI <- contI+num_obj
    contF <- contF+num_obj
    col <- bin[contI:contF]
    mi_df <- cbind(mi_df,col)
  }
  numRow <- 0
  numCol <- 0
  sum <- 0
  while(sum < zeros){
    numRow <- sample(1:num_obj,1)
    numCol <- sample(1:num_attr,1)
    if(mi_df[numRow,numCol] == 1){
      mi_df[numRow,numCol] <- 0
      sum <- sum + 1
    }
  }
  return (mi_df)
}#End putZeros
