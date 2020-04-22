
## This function will produce a special matrix object which can cache 
## its inverted matrix form

makeCacheMatrix <- function(x = matrix()) {
  ## This function takes in a matrix object and outputs a special matrix object
  ## which can store a cached version of its inverted form
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}



##This function can take a special matrix object as input and calculate the inverted
## form of that matrix efficiently. It can be done efficiently because the function 
## checks if there if the inverted matrix has already been calculated and cached in
## the special matrix object.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}


