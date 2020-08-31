## This code calculates the costly matrix inverse operation and stores
## it for quick reference

## Calculates the inverse and stores it in cache.

makeCacheMatrix <- function(x = matrix()) {
  ## Initializing
  M <- NULL
  ## Function to set the Matrix
  setMat <- function(y) {
    x <<- y
    M <<- NULL
  }
  ## Function to get the Matrix
  getMat <- function() x
  ## Function to set the Inverse
  setInv <- function(inverse) M <<- inverse
  ## function to get the Inverse
  getInv <- function() M
  list(setMat = setMat,
       getMat = getMat,
       setInv = setInv,
       getInv = getInv)
}


## Checks if inverse has been calculated and retrieves it from cache.
## If not, calculates the inverse.

cacheSolve <- function(x, ...) {
  ## Return Inverse(x)
  M <- x$getInv()
  
  ## Return cached inverse, if it exists
  if (!is.null(M)) {
    message("Cached Matrix.")
    return(M)
  }

  ## Else, calculate inverse
  else {
    N <- x$getMat()
    M <- solve(N, ...)
    x$setInv(M)
    return(M)
  }
}
