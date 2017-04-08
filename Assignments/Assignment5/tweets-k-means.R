args = commandArgs(trailingOnly=TRUE)

if (length(args)!=4) {
  stop("All four arguments must be supplied (number of clusters, initial seeds, input file, ouput file)", call.=FALSE)
} 

library(jsonlite)
#function: compute the SEE value
getSSE <- function(clusters, df, centroids, output)
{
  sse <- 0
  for(j in 1:k)
  {
    str = paste(j, ":    ", sep = "")
    cat(j)
    cat("  ")
    first = TRUE
    for(i in 1:n)
    {
      if(clusters[i] == j)
      {
        if(first == TRUE){
          first = FALSE
        }else{
          cat(", ")
          str = paste(str, ", ", sep = "")
        }
        ID <- df$id_str[i]
        
        cat(ID)
        str = paste(str, ID, sep = "")
        #add up the sse for cluster j
        temp <- computeJaccardDis(df$text[i], df$text[which(df$id_str == centroids[j])])
        sse <- sse + temp^2
      }
    }
    cat("\n")
    cat("\n")
    write(str, file=output, append = TRUE)
  }
  cat("SSE = ")
  cat(sse)
  write(paste("SSE = ", sse, "\n", sep = ""), file=output, append = TRUE)
  return(sse)
}


#function: compute the Jaccard distance between two tweets
computeJaccardDis <- function(string_x, string_y)
{
  vec_x <- unlist(strsplit(string_x, split = " "))
  vec_y <- unlist(strsplit(string_y, split = " "))
  
  size_of_union <- length(union(vec_x, vec_y))
  size_of_intersection <- length(intersect(vec_x, vec_y))
  
  return(1 - size_of_intersection / size_of_union)
}


#function: nochange(centroids, centroids_new)
nochange <- function(k, centroids, centroids_new)
{
  for(i in 1:k)
  {
    
    if (centroids[i] != centroids_new[i])
    {
      return(FALSE)
    }
    
    
  }
  return(TRUE)
}


#step 0 initialize the parameters
input = args[3]
k = args[1]
initialSeedsFile = args[2]
iteration = 0
output = args[4]

conn = file(input)
#step 1 import the dataset
suppressWarnings(df <- jsonlite::stream_in(conn))
n = nrow(df)
#import the initial seeds
suppressWarnings(seeds <- strsplit(readLines(initialSeedsFile), split = ","))
nseeds = length(seeds)

#step 2 implement the k-means algorithm
#initializing the starting centroids
centroids <- array(data = NA, dim = c(k,1), NULL)
centroids_new <- array(data = NA, dim = c(k,1), NULL)

#centroid[k] is the ID of center k

for(i in 1:k)
{
  #ensure every centroids is in seed file
  centroids[i] = seeds[[(i-1) %% nseeds + 1]]
}

#the core k-means algorithm
clusters <- array(data = NA, dim = c(n, 1), NULL)
convergence = FALSE

while(!convergence && iteration<=100)
{
  iteration <- iteration + 1
  #form clusters based on the centroids
  for(i in 1:n)
  {
    shortest = .Machine$double.xmax
    cluster_no = 0
    
    for(j in 1:k)
    {
      distance <- computeJaccardDis(df$text[i], df$text[which(df$id_str == centroids[j])])
      if(distance < shortest)
      {
        cluster_no <- j
        shortest <- distance
      }
    }
    
    clusters[i] = cluster_no
  }
  
  #get the new centroids
  for(j in 1:k)
  {
    minDis <- .Machine$double.xmax
    new_center_id <- centroids[j]
    #find the minimum sum of distance from one tweet to the centroid of cluster j
    for(i in 1:n)
    {
      
      if(clusters[i] == j)
      {
         sum <- 0
         for(h in 1:n)
         {
           if(clusters[h] == j)
           {
             sum <- sum + computeJaccardDis(df$text[i], df$text[h])
           }
         }
         
         if(sum < minDis)
         {
           minDis <- sum
           new_center_id <- df$id_str[i]
         }
      }
    }
    
    #get the centroids_new 
    centroids_new[j] <- new_center_id
  }
  
  #compare the old centroids and the new ones
  if(!nochange(k, centroids_new, centroids))
  {
    centroids <- centroids_new
  }
  else
  {
    convergence <- TRUE
    
  }
}


#step 3 output the clusters
#compute the sum of squared error
sse <- getSSE(clusters, df, centroids, output)



