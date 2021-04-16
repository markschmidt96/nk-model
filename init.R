##NK-Model Replication from Levinthal - 1997 - Adaption on Rugged Landscapes

##1. Landscape generation 

N = 5 ##N = Number of characteristics (e.g. product characteristics)
K = 0 ##currently only working with no interdependencies (K=0)

landscape = expand.grid(replicate(N, 0:1, simplify = FALSE)) #create all combinations
# landscape = landscape[-1,] # drop first as it contains only 0
# rownames(landscape) = 1:nrow(landscape)
# landscape$index = c(1:nrow(landscape)) 
##zur Info: In dem anderen Projekt sind die Ns Produkt Funktionen, deswegen waren dort 0 Zeilen nicht möglich


gen_fitness <-function(N,K,landscape){
  ##Computes the fitness value for each combination (row in landscape)
  
  
  #generating random fitness values for each N either 0 or 1
  g <- matrix(runif(N * 2^(K + 1)), N) 
  
  #transforming landscape into matrix (required for apply)
  pot = as.matrix(landscape[,1:N])
  #calculating the mean of fitness value for each combination
  fitness = apply(pot, MARGIN = 1, function(x){
    y = unlist(c(x*g[,1],abs(x-1)*g[,2]))
    y = y[y>0]
    ret = mean(y)
    ret
  })
  return(fitness)
}


landscape$fitness = gen_fitness(N,K,landscape)