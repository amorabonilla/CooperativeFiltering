sink("salida1_player3.txt")
FC  <- importing_dataset(FALSE, dataset_selected = 'datasets/pesPlayers2016ModificadoFINAL.csv')
FC
# n number of users
n_users=10
num_scales <- 4
my_support <- 0.009

knowledge <- mining_in_table(FC, my_support,"rules.xml")
Attributes <- knowledge$attributes
Attributes
# Rules will be written in "rules.xml"
usuarios <- list(1:5)
list_users <-usuarios
preferencias <- list(c( "Overall", "Attacking"),
                          c("Age", "Attacking","Overall"),
                          c("Coverage"),
                          c("Height","LoftedPass","Coverage"),
                     c("Catching","Defensive"))
#c("Age","Overall","Weight","Catching","Defensive"))
list_preferences <- preferencias
grados <- list(c(0.5,0.5),
                          c(0,5,0.25,1),
                          c(0.75),
                          c(0.25,1,0.25),
                          c(0.25,0.25))
list_grades <- grados


recommendation <- filtering_features(FC,"rules.xml",usuarios,preferencias,grados)
recommendation$objects
recommendation$next_attributes

