#sink("salida1_coopis3.txt")
FC <- context(8,8,sparness = 0.7)
FC
# n number of users
n_users=3
num_scales <- 4
my_support <- 0.3

knowledge <- mining_in_table(FC,my_support,"rules.xml")
Attributes <- knowledge$attributes
Attributes
# Rules will be written in "rules.xml"
usuarios <- list(1:5)
list_users <-usuarios
preferencias <- list(c("att1","att3","att5"),
                          c("att2","att3","att5"),
                          c("att3"),
                          c("att1","att2","att4"),
                          c("att1"))
list_preferences <- preferencias
grados <- list(c(0.25,0.5,1),
                          c(0,5,1,1),
                          c(0.75),
                          c(1,1,1),
                          c(0.25))
list_grades <- grados

name_file_rules <- "rules.xml"
list_users <- usuarios
list_preferences <- preferencias
list_grades <- grados
 
recommendation <- filtering_features(FC,"rules.xml",usuarios,preferencias,grados)
recommendation$objects
recommendation$next_attributes

