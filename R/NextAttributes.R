Next.attributes <- function(Preferences,trace=FALSE){
  # browser()
  X <- attributes.from.Next(Preferences$Next)
  if (trace) print.initial.sets(X,Preferences$implications) 
  Next_Preferences <- next.min.gen(X,Preferences$implications)
  if (trace) print.preferences(Next_Preferences) 
  return(Next_Preferences)
}# function



preference.selection <- function(Rules,trace=FALSE){
  # Base  <- duquenne.basis(FC) 
  # Rules <- Base$DGbasis
  #  Attributes <- attributes.from.rules(Rules)
#  browser()
  Attributes <- Rules@lhs@itemInfo$labels
  #  Attributes_sp <- vector.to.sparse(Attributes)
  #  Attributes <- Base$attributes
  initialSet <- attributes.from.table(Attributes)
#  X <- set.to.sparse(initialSet, Attributes)
  X <- initialize.setOfAttributes(initialSet, Attributes)
 
  if (trace) print.initial.sets(X,Rules) 
  preferences <- next.min.gen(X,Rules)
  if (trace) print.preferences(preferences) 
  while  (!is.null(preferences$Next)){
    preferences <- Next.attributes(preferences,trace=TRUE)
  }
  
  return(preferences)
} 

print.initial.sets <- function(X,Rules){
  cat("***** Initial sets....:\n*****\n")

  if (is.null(X)){ 
    cat("X NULL\n")
  } else {inspect(X)}
  
  if (length(Rules)==0){ 
    cat("Initial set of implications empty\n ")
  } else {
    cat("Initial set of implications\n\n")
    inspect(Rules)
  } 
  cat("\n\n")
  
}
print.preferences <- function(Next_Preferences){
  cat("***** Next Preferences*****\n")
  if (is.null(Next_Preferences)) {
    cat("    ALL preferences are NULL \n")
    return()
  }
  cat(" ===== After NextMinGen =====\n")
  if (is.null(Next_Preferences$closure)){ 
    cat("    Closure computed is NULL \n")
  } else {
    cat("    Closure computed:\n\n")
    inspect(Next_Preferences$closure)
  }
  
  cat("------\n\n")
  
  if (length(Next_Preferences$implications)==0){ 
    cat("Implications returned empty\n")
  } else {
    cat("    Implications returned: \n")
    inspect(Next_Preferences$implications)
    cat("------\n\n")
  }
  cat("------\n\n")
  
  if (is.null(Next_Preferences$Next)){ 
    cat("    Next Preferences Candidates are NULL \n\n")
  } else {
    cat("    Next Preferences Candidates:\n\n")
    inspect(Next_Preferences$Next)
  }
  cat("*****  end iteration NextMingen------\n\n")
  
}
