library(foreach)

nk_landscape <-function(N,K){
  
  set.seed(13)#reproducibility
  
  if(K < 0){ K = 0}
  if(K >= N){ K = N - 1}
  
  landscape = expand.grid(replicate(N, 0:1, simplify = FALSE)) #create all combinations
  
  
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
  
  return(landscape)
  
  
}



