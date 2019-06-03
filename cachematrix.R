## These functions are used to calculate the inverse of a matrix.  
## Once the inverse of a matrix has been calculated, it is stored in
## a cache, if that same matrix is used again then instead of 
## calculating the inverse again, these functions will return the
## relevant value stored in the cache


## makeCacheMatrix sets up a list to act as a cache.  It defines 4 
## other functions which are used by the cacheSolve function to 
## check the cache to see if the matrix inverse has already been
## calculated

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverted <- function(solve) m <<- solve
  getinverted <- function() m
  list(set = set, get = get,
       setinverted = setinverted,
       getinverted = getinverted)
  
}


## cacheSolve returns a matrix that is the inverse of x.  If 
##the inverse has previously been created then it returns the value
##from the cache otherwise it calculates it and stores it in the cache

cacheSolve <- function(x) {
  m <- x$getinverted()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverted(m)
        m
}
