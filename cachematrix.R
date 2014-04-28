## Construct function with list of sub-functions. Second function checks if inverse has been calculated before (cached). If it is, then retrieve. If not, then calculate the inverse 1) save to cache, 2) return inverse matrix

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {               #Create a function that sets input matrix to x
    x <<- y
    m <<- NULL
  }
  get <- function() x                #Create get function to retrieve x matrix
  setinv <- function(inv) m <<- inv  #set inverse matrix to "m"
  getinv <- function() m             #get inverse matrix from "m"
  list(set = set, get = get,         #construct a list of functions
       setinv = setinv,
       getinv = getinv)
}


## computes the inverse of the special matrix x. If the inverse has already been calculated, then retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
  m <- getinv()          #see if x is cached
  if(!is.null(m)) {      #if cache exist
    message("getting cached data")
    return(m)            #return the cached inverse matrix
  }
  data <- x$get()        #if cache doesn't exist
  m <- solve(data, ...)  #calculate the inverse matrix
  x$setinv(m)            #save result to cache
  m                      #return inverse matrix
}
