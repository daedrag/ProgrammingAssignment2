## Assignment: Caching the Inverse of a Matrix
## Cache the inverse of a matrix rather than compute it repeatedly


## Create a special "matrix" object that can cache its inverse
## Contain a list of get/set of matrix 'x' and its inverse 'm'

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInv <- function(inv) m <<- inv
    getInv <- function() m
    list(set = set, 
         get = get,
         setInv = setInv,
         getInv = getInv)

}



## Compute the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
    ## Check if the inverse has already been calculated and retrieve 
    ## Assume x not changed 
    m <- x$getInv()  
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    ## If inverse is not calculated,
    ## then get and solve
    ## Assume matrix 'data' always invertible (stated in assgmt description)
    data <- x$get()  
    m <- solve(data, ...)
    x$setInv(m)
    
    ## Return the inverse of 'x'
    m
  
}

## Test
## > c=rbind(c(1, -1/4), c(-1/4, 1))
## > cacheSolve(makeCacheMatrix(c))
