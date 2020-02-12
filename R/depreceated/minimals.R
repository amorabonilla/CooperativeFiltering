L <- IS$get_LHS_matrix()
M <- fcaR:::.subset(L)
ind <- which(colSums(M)==1)