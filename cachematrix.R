#An initial consideration should be to use matrix() objetct instead of the numeric() object shown on the provided example

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  #calculate the inverse of the matrix (solves it)
  setInverse <- function() inv <<- solve(x) 
  getInverse <- function() inv
  
#returns the list of cache values 
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



#This next portion of the code is similar to the one provided in the example, but the setInverse and getInverse variables are specified 

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting inverse of Matrix")
    return(m)
  }
  
  #This part of the code calcualtes the mean after the IF evaluation determines m is null and goes to generate the inverse. 
  data <- x$get()
  m <- mean(data, ...)
  x$setInverse(m)
  m
