# A pair of function that compute and cache the inverse on an invertible matrix 

## Generate a special matrix that caches its inverse

makeCacheMatrix <- function(m = matrix()) {
  ## Initiate the inverse property
  i <- NULL
  ## Setter and getter
  set <- function(mat) {
    m <<- mat
    i <<- NULL
  }
  
  get <- function() m
  
  ## Setter and getter for the inverse
  set.inverse <- function(inverse) i <<- inverse
  
  get.inverse <- function() i
  
  ## Return a list of defined methods
  list(set = set, get = get, set.inverse = set.inverse, 
       get.inverse = get.inverse)
}


## Compute the inverse of the generated matrix, if already computed 
## return the cached value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get.inverse()
  
  ## Return the inverse if it's there
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Otherwise get the data
  data <- x$get()
  
  ## Calculate and set the inverse
  m <- solve(x) %*% x
  x$set.inverse(m)
  
  ## Return the inverse
  m
}
