## This file contains two function makeCacheMatrix() and cachesolve(). 
## makeCacheMatrix() creates R object that stores a matrix and its inverse
## cachesolve() requires an argument that is returned by makeCacheMatrix() to retreve the inverse 
## cached value that is stored in the makeCacheMatrix() object's environment

##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
  {
    m <- NULL
    set <- function(y) 
      {
        x <<- y
        m <<- NULL
      }
    get <- function() x
    setsolve <- function(sol) m <<- sol
    getsolve <- function() m
    list(set = set, get = get,setsolve = setsolve, getsolve = getsolve)
  }

## This function computes the inverse of the special "matrix" 
##returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
##`cacheSolve` retrieve the inverse from the cache

cacheSolve <- function(x, ...) 
  {
    m <- x$getsolve()
    if(!is.null(m)) 
      {
        message("getting cached data")
        return(m)
      }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m) 
    m  ## Return a matrix that is the inverse of 'x'
  }


#makeVector <- function(x = numeric()) {
#  m <- NULL
#  set <- function(y) {
#    x <<- y
#    m <<- NULL
#  }
#  get <- function() x
#  setmean <- function(mean) m <<- mean
#  getmean <- function() m
#  list(set = set, get = get,
#       setmean = setmean,
#       getmean = getmean)
#}

#cachemean <- function(x, ...) {
#  m <- x$getmean()
#  if(!is.null(m)) {
#    message("getting cached data")
#    return(m)
#  }
#  data <- x$get()
#  m <- mean(data, ...)
#  x$setmean(m)
#  m
#}
