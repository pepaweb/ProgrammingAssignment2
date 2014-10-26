## function that creates an object with 4 functions
## to: 
## set the matrix content
## get the matrix content
## set the matrix inverse
## get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) i <<- solve(x)
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function retrieves the result of getinverse function
## if inverse matrix is already calculated then the cached information
## is retourned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i  
}
