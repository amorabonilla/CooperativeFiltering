importing_dataset <- function(random=FALSE,objects=NULL, 
                              attributes=NULL, sparness_degree=NULL, 
                              dataset_selected=NULL){
  if (random){
    FC <- context(objects,attributes,sparness_degree)
  } else {
#    FC <- read.csv(dataset_selected,sep=";")
    FC <- read.csv(dataset_selected)
  }
#  FC <-as.data.frame(sapply(FC, function(x) as.logical(x)))
  FC$X <- NULL
  return(FC)
}