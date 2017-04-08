args = commandArgs(trailingOnly=TRUE)

if (length(args) != 3) {
  stop("You have to provide exact three arguments (number of clusters, input file, output file)", call.=FALSE)
} 

# function: computeDis(Src_X, Src_Y, Dst_X, Dst_Y) 
# We are using euclidean distance
computeDis <- function(Src_X, Src_Y, Dst_X, Dst_Y)
{
  r = sqrt((Src_X-Dst_X)^2 + (Src_Y-Dst_Y)^2)
  return (r)
}

#function: compute the SEE value
getSSE <- function(clusters, df, centroids, output)
{
  n = nrow(df)
  sse <- 0

  for(j in 1:k)
  {
    
    cat(j)
    cat("  ")
    str = paste(j, "    ", sep = "")
    
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
        cat(i)
        str = paste(str, i, sep = "")
        
        #add up the sse for cluster j
        temp <- computeDis(df$x[i], df$y[i], centroids[j, 1], centroids[j, 2])
        sse <- sse + temp^2
      }
    }
     cat("\n")
     write(x= str , file = output, append =TRUE)
    
  }
  cat("SSE = ")
  cat(sse)
  
  write(x= paste("SSE = ", sse, "\n", sep = "") , file = output, append =TRUE)
  return(sse)
  
}

#function: nochange(centroids, centroids_new)
nochange <- function(k, centroids, centroids_new)
{
  for(i in 1:k)
  {
    diff <-computeDis(centroids[i,1], centroids[i,2], centroids_new[i, 1], centroids_new[i, 2])
    if (diff > 0.0001)
    {
      return(FALSE)
    }
      
    
  }
  return(TRUE)
}

#step 0 initialize the parameters
input = args[2]
output = args[3]
k = as.integer(args[1])
iteration = 0

#step 1 import the dataset
df = read.table(input, header=TRUE)
n = nrow(df)
names(df) = c('id', 'x', 'y')

#step 2 implement the k-means algorithm
#initialize the starting centroids
if(k>n){
  stop("K is greater than the total number of entries. Please try a smaller k.")
}
starting_points <- sample(1:n, k, replace = FALSE)
centroids <- array(data = NA, dim = c(k,2), NULL)
centroids_new <- array(data = NA, dim = c(k,2), NULL)

#centroid[k, 1] is the x coordinate of center k
#centroid[k, 2] is the y coordinate of center k
for(i in 1:k)
{
  
  centroids[i, 1] = df$x[starting_points[i]]
  centroids[i, 2] = df$y[starting_points[i]]
  
}
#make the NULL accumulator
null_accumulator <- array(data = NA, dim = c(k, 3), NULL)
for(i in 1:k)
{
  null_accumulator[i,1] <- 0
  null_accumulator[i,2] <- 0
  null_accumulator[i,3] <- 0
  
}

#the core k-means algorithm
clusters <- array(data = NA, dim = c(n, 1), NULL)
convergence = FALSE

while((!convergence) && (iteration <= 25))
{
  iteration <- iteration + 1
  
  #accumulator[k,1] is the sum of x coordinate of points in cluster k
  #accumulator[k,2] is the sum of y coordinate of points in cluster k
  #accumulator[k,3] is the number of points in cluster k
  
  accumulator <- null_accumulator
  #form clusters based on the centroids
  for(i in 1:n)
  {
      shortest = .Machine$double.xmax
      cluster_no = 0
      
      for(j in 1:k)
      {
        distance = computeDis(df$x[i], df$y[i], centroids[j,1], centroids[j,2])
        if(distance < shortest)
        {
          cluster_no = j
          shortest = distance
        }
      }
      
      clusters[i] = cluster_no
      accumulator[cluster_no, 1] <- accumulator[cluster_no, 1] + df$x[i]
      accumulator[cluster_no, 2] <- accumulator[cluster_no, 2] + df$y[i]
      accumulator[cluster_no, 3] <- accumulator[cluster_no, 3] + 1
      
  }
  
  #get the new centroids
  for(j in 1:k)
  {
    centroids_new[j, 1] <- accumulator[j, 1] / accumulator[j, 3]
    centroids_new[j, 2] <- accumulator[j, 2] / accumulator[j, 3]
  }
  
  #check if the centroids have changed since the last iteration. Update convergence to TRUE if centroids have not changed
  if(nochange(k, centroids, centroids_new))
  {
    convergence = TRUE
  }
  else
  {
    centroids <- centroids_new
  }
}


#step 3 output the clusters
#compute the sum of squared error
sse <- getSSE(clusters, df, centroids, output)




