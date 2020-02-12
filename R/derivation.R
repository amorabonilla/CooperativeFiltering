intent <- function(context,my_attr){
#   browser()
  att <- colnames(context)[as.logical(my_attr$get_vector())]
  if (is.null(att)|length(att)==0) return(rownames(context))
  objects <- context %>% dplyr::select(att) %>% apply(1,function(x) all(as.logical(x)))
#  browser()
  return(rownames(context)[which(objects)])
}  

 

extent <- function(context,my_obj){
  # browser()
  if (is.null(my_obj)|length(my_obj)==0) return(colnames(context))
  sub_df <- context[my_obj,]
  if (!is.data.frame(sub_df)) 
    return(names(context[which(as.logical(sub_df))]))
  else{
    attrib <- apply(sub_df,2,function(x) all(x))
    return(names(which(attrib)))
  }
}  


intent_old <- function(context,my_attr){
  #   browser()
  if (is.null(my_attr)) return(rownames(context))
  sub_df <- context[,my_attr]
  if (!is.data.frame(sub_df)) 
    return(rownames(context[which(as.logical(sub_df)),]))
  else{
    obj <- apply(sub_df,1,function(x) all(x))
    return(names(which(obj)))
  }
}  