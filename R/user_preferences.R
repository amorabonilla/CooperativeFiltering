#simulate interaction with users
user_preferences <- function(Attributes,n,num_scales){
#  browser()
  dt_preferences <- data.frame(user=1:n)
  dt_preferences <- dt_preferences %>%
    mutate(preferences =lapply(user,
                               function(x) attributes_from_table(Attributes))) 
  
  dt_preferences <- dt_preferences %>%
    mutate(grades=lapply(preferences,function(x) {
      random_grades(length(x),num_scales)}))
  #              top_n(k, grades) %>%
  #              arrange(desc(grades))
  
  return(dt_preferences)
}
