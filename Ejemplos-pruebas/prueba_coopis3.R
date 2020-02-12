#sink("salida1_player3.txt")
FC  <- importing_dataset(FALSE, dataset_selected = 'datasets/context.csv')
FC
# n number of users
n_users=5
num_scales <- 4
my_support <- 0.2

knowledge <- mining_in_table(FC, my_support,"rules.xml")
Attributes <- knowledge$attributes
Attributes
# Rules will be written in "rules.xml"
usuarios <- list(1:2)
list_users <-usuarios
preferencias <- list(c( "att4", "att1"),
                     c("att1","att5"))
#c("Age","Overall","Weight","Catching","Defensive"))
list_preferences <- preferencias
grados <- list(c(0.5,0.5),
                          c(0.25,0.25))
list_grades <- grados


recommendation <- filtering_features(FC,"rules.xml",usuarios,preferencias,grados)
recommendation$objects
recommendation$next_attributes

