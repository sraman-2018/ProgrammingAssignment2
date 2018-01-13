## Put comments here that give an overall description of what your
## functions do

## The purposes of these functions is to 
## 1. get introduced to the <<- operator which can be used to assign a value to an object in 
##    an environment that is different from the current environment. In other words it is like having global variables that be
##    accessed by another environment. For this we will create a function that can 
## 2. Using the above concept avoid repeated time concuming oprations by caching the result if the data set being used is static and
##    does not change.
## To illustrate this we have function to cache the inverse of a matrix and a function to return the inverse of the matrix from 
## the cache if it has already been computed or if it has not yet been computed then compute the result and have it cached.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(m = matrix(numeric(0), 0,0)) {
    im <- NULL
    set <- function(y) {
      m <<- y
      im <<- NULL
    }
    get <- function() m # Return original Matrox
    setInvMatrix <- function(m) im <<- m
    getInvMatrix <- function() im
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}

## Write a short comment describing this function
## his function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {    ## Return a matrix that is the inverse of 'x'
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m
}

## Now test our Solution
mat <- matrix(1:4, 2,2)
function_list <- makeCacheMatrix(mat)
inv_mat <- cacheSolve(function_list) # First Time solving Inverse
inv_mat  ## Print the Inverse Matrix
cacheSolve(function_list) # This time it should get from get from cache
# Now get the Original Matric back by doing thr inverse of its inverse
cacheSolve(makeCacheMatrix(inv_mat))


