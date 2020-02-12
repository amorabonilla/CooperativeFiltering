FC <- context(10,10,0.5)

FC1 <-as.data.frame(sapply(FC, function(x) as.logical(x)))
#names(FC) <- c("AireLibre","Fumar","Terraza","Familias","RestCaro","BuenasVistas","Niños")
Rules <- apriori(FC1,parameter = list(confidence=1,support=0.2))

fc <- formal_context$new(FC)

# add implications
fc$add_implications(Rules)

# Compute implications
#fc$extract_implications_concepts(verbose = FALSE)

# Cardinality and mean size in the ruleset
fc$implications$cardinality()
#> [1] 7
sizes <- fc$implications$size()
colMeans(sizes)

A1 <- c("att1","att3")
A11 <- to.sparse(A1,fc$attributes)

cl <- fc$implications$compute_closure(A11,reduce = TRUE)
closure1 <- from.sparse(cl$closure,fc$attributes)
closure1
Imp1 <- implication_set$new(attributes = fc$attributes,lhs=cl$implications$lhs,rhs=cl$implications$rhs)
Imp1

