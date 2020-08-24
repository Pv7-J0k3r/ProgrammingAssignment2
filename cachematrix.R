## This code calculates the costly matrix inverse operation and stores
## it for quick reference

## Calculates the inverse and stores it in cache.

makeCacheMatrix <- function(x = matrix()) {
  M <- NULL
  setM <- function(y) {
    x <<- y
    M <<- NULL
  }
  getM <- function() x 
  setC <- function(inverse) M <<- inverse
  getC <- function() M
  list(setM = setM,
       getM = getM,
       setC = setC,
       getC = getC)
}


## Checks if inverse has been calculated and retrieves it from cache.
## If not, calculates the inverse.

cacheSolve <- function(x, ...) {
  M <- x$getC()
  
  if (!is.null(M)) {
    message("Cached Matrix.")
    return(M)
  }

  else {
    N <- x$getM()
    M <- solve(N, ...)
    x$setC(M)
    return(M)
  }
}
