## Below are a pair of functions that that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    # use of `<<-` to assign a value to an object in an environment 
    # different from the current environment
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) I <<- inverse
  getInverse <- function() I
  list( set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)

}


##  This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getInverse()
  # if the inverse has already been calculated
  if(!is.null(I)){
    # get inverse of matrix from the cache and skips the computation
    message('Getting cached data')
    return(I)
  }
  # otherwise, calculates the inverse of the matrix
  m <- x$get()
  I <- solve(m, ...)
  # sets the value of the I in the cache via the setInverse function
  x$setInverse(I)
  I
}
