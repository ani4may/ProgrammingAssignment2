
## makecachematrix initilizes an object or a new matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list( set = set , get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## cachesolve just solves the value of the matrix to return the inverse
## nothing much to describe except I changed the vector example by using solve() and 'i' in place of m

cacheSolve <- function(x, ...) {
  
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  d<-dim(data)
  cond<-((d[1]) == (d[2]))   ## tests if matrix is square
  if(cond)                  
  {
    i <- solve(data, ...)    ## solve function directly calculates inverse of a square matrix
    x$setinverse(i)
    i
  }
  else{message("not a square matrix")}  ## message when not a square matrix
}

## steps to use function : 1. your_new_cache_matrix <- makeCacheMatrix( pass whatever matrix you like here)
##                         2. call cachesolve(your_new_cache_matrix)
##                         3. Voila!