## These functions take a matrix and cache it later calculate the inverse
## matrix. This makes it so the inverse matrix is solved only once and will
## be stored. With larger sets of data, this is more efficient.

## This function creates a special "matrix" object that can cache its inverse
## 'x' is an invertible matrix containing real numbers

makeCacheMatrix <- function(x = matrix()) {
  i <- x
  
  set <- function(y) {
    x <<- y
    i <<- x
  }
  
  get <- function() x
  input <- i
  setInv <- function(inverse) i <<- inverse
  getInv <- function() i
  
  list(set = set, get= get, setInv = setInv, getInv = getInv, x = i)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache
  
## 'x' is a special 'matrix' that has been returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    i <- x$getInv()

  if(!is.null(i)) {
    message("getting cached data")
    
    data <- i
    i <- solve(data)

    return(i)
  }
        ## Return a matrix that is the inverse of 'x'
}
