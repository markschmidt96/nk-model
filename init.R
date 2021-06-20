##NK-Model Replication from Levinthal - 1997 - Adaption on Rugged Landscapes

library("foreach")
library("stringr")
library("philentropy")
library('MASS')
library("scatterplot3d") # load

##1. Landscape generation

N =8 ##N = Number of characteristics (e.g. product characteristics)
K = 7  ## Number of interdependencies

landscape = nk_landscape(N,K)


dmatrix <- as.matrix(distance(subset(landscape, select = -fitness), method = "euclidean")^2)




#climb_index is the climbing point of the hill climb


#hill_climb <- function(landscape, climb_index=N/2){


#  pot = as.matrix(landscape[,1:N])

#  y <- c(pot[,N])

#  l <- climb_index - 1
#  r <- climb_index + 1

#  if(l < 0) {yl <- 0} else {yl <- as.numeric(y[l])}
#  if(r > N) {yr <- 0} else {yr <- as.numeric(y[r])}

# yi <- as.numeric(y[climb_index])

# if ( yi >= yl && yi >= yr){
#   return(climb_index)
# }

# if(yl > yr) {

#    return(hill_climb(landscape, l))
#  } else {

# return(climb(y,r))

# }

#}



#temp <- str_replace(temp,"v","")
#landscape[pos,N+1]

#print(paste(landscape[pos,1:N]))

#print("Fitness at start:")          
#print(paste(landscape[pos,N+1]))

#print("Fitness at end:")
#print(paste(landscape[n,N+1]))



agent_history <- matrix(nrow = 2^N, ncol = 4)

agent_count <- 100
iteration <- 0

agent_matrix <- matrix(nrow = 100,ncol=3)		#Matrix with agents, their position and status
agent_matrix[,1] <- 1:100
agent_matrix[,3] <- 1

agent_id = 1:100

foreach(i=agent_id) %do%{				#Random starting position
  agent_pos = sample(1:2^N, 1)
  agent_matrix[i,2] = agent_pos
}

visited_pos <- matrix(nrow=2^N,ncol=2)			#Matrix to track visited organisations
visited_pos[1:2^N,1] = 1:2^N
visited_pos[1:2^N,2] = 0


#Create distance matrix using the "simple matching method" which is (euclidean)^2
#The matrix displays the distance between each position
distantmatrix <- as.matrix(distance(subset(landscape, select = -fitness), method = "euclidean")^2)




#Add fitness column into distantmatrix
distantmatrix <- cbind(fitness=landscape$fitness,distantmatrix)
#matrix(distantmatrix, nrow = 2^N, ncol= 2^N)
#distantmatrix[distantmatrix[,2^N] ==1,]

stable_agents = FALSE

while (stable_agents == FALSE) {
  
  foreach(l=agent_id) %do% {
    
    if(agent_matrix[l, 3] == 1) {
      
      #Define starting position, pos(1,...,2^N)
      pos <- agent_matrix[l, 2]
      
      #
      hill <- landscape[pos,N+1]
      
      longjump <- 0
      
      change <- FALSE
      
      #Identify and select fitness values of neighbor positions dependent on starting position
      local_set <- subset(distantmatrix, distantmatrix[,pos+1]==1) #neighbors of 1 simple matching distance
      
      
      #Initialize local search
      k <- 1  #Counter
      
      #Compare fitness of starting position with fitness of neighbor; starting with the first row
      
      neighbors <- rownames(local_set)
      
      while (k <= N) #Testing whether any neighbor is larger
      {
        
        if(hill < local_set[k,1]){
          hill <- local_set[k,1]
          temp <- neighbors[k]
          temp <- str_replace(temp,"v","")
          pos <- as.integer(temp)
          change <- TRUE
        }
        
        k <- k + 1
        
        
        
        
      }
      
      longjump <- sample(1:2^N,1)
      if (hill < landscape[longjump, N+1]){
        hill <- landscape[longjump, N+1]
        pos <- longjump
        change <- TRUE
        
      }
      
      
      if(visited_pos[pos,2] == 1){ #If a dominant Neighbor already visited terminate agent
        agent_matrix[l,3] = 0
      }
      if(change == FALSE){  #If local maximum reached
        agent_matrix[l,3] = 2
      }
      
      visited_pos[agent_matrix[l,2],2] = 1
      visited_pos[pos,2] = 1
      agent_matrix[l,2] = pos
    } 
  }
  
  status_dead = 0 # status 0
  status_unfinished = 0 # status 1
  status_stable = 0# status 2
  
  
  foreach(q = agent_id) %do% {
    status <- agent_matrix[q,3]
    
    if(status == 0){
      status_dead = status_dead + 1
    }
    if(status == 1){
      status_unfinished = status_unfinished + 1
    }
    if(status == 2){
      status_stable = status_stable + 1
    }
  }
  
  if(status_unfinished == 0){	#If all agents finished or dead
    stable_agents = TRUE
  }
  iteration = iteration + 1
  
  agent_history[iteration, 1] = iteration
  agent_history[iteration, 2] = status_dead
  agent_history[iteration, 3] = status_unfinished
  agent_history[iteration, 4] = status_stable
  
  
  
}