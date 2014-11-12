# The function "makeCacheMatrix" stores the value of a matrix and its inverse,  
# so that the inverse can be calculated once. The function "cacheSolve" calculate the 
# inverse and store it

makeCacheMatrix <- function(x = matrix()) {
  # Create a matrix object that allow to store and retrive it's inverse
  # Initiate the inverse value 
  inv <- NULL
  # set the valuse of the matrix and its initial inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # retrive the matrix 
  get <- function() x
  # Retrive the matrix inverse
  getinverse <- function() inv
  # Assign the inverse to the enviroment
  setinverse <- function(i) inv <<- i
  # Creat a way to call the different methods
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  # Return the matrix x inverse
  # first check if x inverse is in enviroment
  i <- x$getinverse()
  if (!is.null(i)) {
    message("Getting cashed inverse")
    return(i)
  }
  # If the inverse is not in the enviroment, calculate it
  data <- x$get()
  inv <- solve(data)
  # store in enviroment and return it
  x$setinverse(inv)
  inv
}
