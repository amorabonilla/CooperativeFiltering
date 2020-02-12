# computing similarities
library(tidyverse)
 
library(magrittr)
eclosure <-  read.csv("output/new1__closure.csv")
 

v_executions <- eclosure %>% 
  pull(execution)%>% 
  unique()

v_experiments <- eclosure %>% 
  pull(experiment)%>% 
  unique()

v_iterations <- eclosure %>% 
  pull(iteration)%>% 
  unique()

eclosure2 <- list_exec_exp <- eclosure %>% 
  mutate(exec_exp=paste(execution,experiment)) %>% 
  select(exec_exp,closure)

list_exec_exp <- eclosure2 %>%  
  select(exec_exp) %>%
  unique 

list_closure <- as.list(list_exec_exp)$exec_exp %>%  
    lapply(function(x){
       eclosure2 %>% 
        filter(exec_exp == x) %>%
        select(closure)%>%  unique  
    })

dt_all_closures <-
  mutate
  as.list(list_exec_exp)$exec_exp 
  #%>%  
#  function(x){
#    
#  }
