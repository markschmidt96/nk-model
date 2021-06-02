##NK-Model Replication from Levinthal - 1997 - Adaption on Rugged Landscapes

##1. Landscape generation

#execute foreach library
library(foreach)

N = 10 ##N = Number of characteristics (e.g. product characteristics)
K = 2 ##currently only working with no interdependencies (K=0)

landscape = expand.grid(replicate(N, 0:1, simplify = FALSE)) #create all combinations
# landscape = landscape[-1,] # drop first as it contains only 0
# rownames(landscape) = 1:nrow(landscape)
# landscape$index = c(1:nrow(landscape))
##zur Info: In dem anderen Projekt sind die Ns Produkt Funktionen, deswegen waren dort 0 Zeilen nicht möglich

## old_version

#gen_fitness <-function(N,K,landscape){
##Computes the fitness value for each combination (row in landscape)


#generating random fitness values for each N either 0 or 1
#g <- matrix(runif(N * 2^(K + 1)), N)

#transforming landscape into matrix (required for apply)
#pot = as.matrix(landscape[,1:N])
#calculating the mean of fitness value for each combination
#fitness = apply(pot, MARGIN = 1, function(x){
#y = unlist(c(x*g[,1],abs(x-1)*g[,2]))
#y = y[y>0]
#ret = mean(y)
#ret
#})
#return(fitness)
#}

##new version
# idx is the index vector for all N variables
# M is the vector accounting for the K previous variables

gen_fitness <- function(N,K,landscape) {
  
  #generating random fitness values for each N either 0 or 1
  g <- matrix(runif(N*2^(K+1)),N)
  
  #initializing index vector
  idx <- 1:N
  
  #initializing M vector
  M <- 1:K
  
  #Landscape turned into matrix
  pot = as.matrix(landscape[,1:N])
  
  #calculating the respective fitness values
  fitness = apply(pot, MARGIN = 1, function(x){
    usum = 0

        #foreach package required
    usum0 = foreach(i = idx,.combine=c) %do% {
      if(K != 0) {
        
        xx <- x[unique(sort(c(i, ((i + M - 1)%%N) + 1)))]
      } else {
        xx <- x[i]
      }
      g[i, sum(2^(0:(length(xx)-1)) * xx)+1]
    }
    usum = sum(usum0)
    usum = usum/N
    usum
  })
  
  return(fitness)
}

landscape$fitness = gen_fitness(N,K,landscape)


#climb_index is the climbing point of the hill climb


hill_climb <- function(landscape, climb_index=N/2){
  
  
  pot = as.matrix(landscape[,1:N])
  
  y <- c(pot[,N])
  
  l <- climb_index - 1
  r <- climb_index + 1
  
  if(l < 0) {yl <- 0} else {yl <- as.numeric(y[l])}
  if(r > N) {yr <- 0} else {yr <- as.numeric(y[r])}
  
  yi <- as.numeric(y[climb_index])
  
  if ( yi >= yl && yi >= yr){
    return(climb_index)
  }
  
  if(yl > yr) {
    
    return(hill_climb(landscape, l))
  } else {
    
    return(climb(y,r))
    
  }
  
}