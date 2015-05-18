## Two functions that work together to 
##    makeCacheMatrix: store and/or retrieve a matrix and its inverse in cache memory
##    cacheSolve     : retrieves (and saves in cache) the inverse of the matrix if not already in cache

## Creates an object that saves and retrieves the value of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
   # Initialize variables
   #
   m <- NULL
   #
   # Implement the storing and retrieving ot the target matrix
   #
   setmatrx <- function(y) {
      x <<- y
      m <<- NULL
   }
   getmatrx <- function() x
   #
   # Do the same for the inverted target matrix (assumed to be definite positive)
   setmtrxinv <- function(solve) m <<- solve
   getmtrxinv <- function() m
   # output a list
   #
   list(setmatrx = setmatrx, getmatrx = getmatrx,
        setmtrxinv = setmtrxinv,
        getmtrxinv = getmtrxinv)
   
}


## Inverts a matrix (assumed to be definite positive) 
## only if the resultant inverse matrix has not already been calculated and cached

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   # Initialize variables
   #
   # Fetch whatever may have been saved in the cache
   m <- x$getmtrxinv()
   #
   # If something is found then use (and advertise) it...
   if(!is.null(m)) {
      message("Retrieving cached inverse matrix")
      return(m)
   }
   # otherwise  retrieve the original matrix data
   # invert it, cache it and return it
   data <- x$getmatrx()
   m <- solve(data, ...)
   x$setmtrxinv(m)
   m
}