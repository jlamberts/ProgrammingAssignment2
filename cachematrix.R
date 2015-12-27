##cachematrix.R consists of two functions: makeCacheMatrix and cacheSolve

##  makeCacheMatrix takes a single matrix as an argument and returns a list of
##  four functions that get and set the matrix and the inverse respectively

##  cacheSolve takes a makeCacheMatrix list and checks to see whether or not
##  the list has an already calculated inverse.  if it does, it returns the cached
##  matrix.  if not, it calculates the inverse and caches it for later

##the structure for this package is based on the Coursera example which caches
##the mean of a vector

## This function takes a single matrix as an argument and returns a cached matrix
## object

makeCacheMatrix <- function(x = matrix()) {
i <- NULL  
##initially set the inverse to null
set <- function(y){ 
  ##setter function allows changing of the matrix
  x <<- y
  i <<- NULL
  ##double arrows are used to apply the changes to the parent scope 
  ##(the cached matrix object).  if the matrix is changed, the inverse 
  ##must be reset (i<<-Null)
}

get <- function() x
##getter to return the matrix
setInverse <- function(inv) i <<-inv
##setter to change the cached inverse
getInverse <- function() i
##getter to return the cached inverse (or null if there is no cache)
list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
##return a list of functions.  because of lexical scoping, these functions will
##be able to access the matrix and inverse variables as defined in makeCacheMatrix()
}


## This function takes a cached matrix object (created by makeCacheMatrix) and
## returns the inverse of the matrix.  it first checks for a cached inverse, and
## if one does not exist it calculates and stores it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  ## pull inverse from the cached matrix object.  if none is define, this will be
  ## NULL.
  if(!is.null(i)) {
    ##if i is not null, a cached matrix exists.  inform the user and return that
    ##cached value
    message("cached inverse found: getting cached inverse")
    return(i)
  }
  
  ##if there is no cached inverse, this block of code executes:
  data <- x$get()
  ##get the matrix from the cached matrix object
  I <- solve(data, ...)
  ##calculate the inverse using the matrix
  x$setInverse(I)
  ##store the inverse into the cache for later use
  message("no cached inverse found: calculating inverse and caching")
  ##inform the user that no cache was found
  I
  ##return the calculated inverse
}
