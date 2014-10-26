## This is a global variable to cache the result
cache<-list()

## This function will store matrix , along with it's inverse in cache
makeCacheMatrix <- function (x,invX)
{
  ## Calculating the length of cache, in order to reach the end.
  len<-length(cache)  
  ## inserting at the end of the cache ( both matrix and inverse)
  cache[[len+1]]<<-list(x,invX)
}

cacheSolve <- function(x)
{
 ## Calculating the length of the cache, intializing the counter and return matrix
  len<-length(cache)+1
  i<-1
  y<-matrix()
 
## In case the cache is empty we always need calculate the inverse
## and subsequently store it in the cache
  if (len<=1)
  {
    ##print("Calculating fresh")
    y<-solve(x)
    makeCacheMatrix(x,y)
    return(y)
  }
 
## Searching if it is available in the cache
## If yes , returning it.
  while ( i < len && len >1)
  {
    z<-cache[[i]][[1]]
    if (identical(z,x) )
    {
      y<-cache[[i]][[2]]
      return(y) ## Program terminates here since match found
    }
    i<-i+1
  }

## Not found in the cache hence calculating the inverse 
## and subsequently storing it in the cache
y<-solve(x)
##print(y)
makeCacheMatrix(x,y)
  
## returing the inverse , which is either retrieved or calculated
y

}