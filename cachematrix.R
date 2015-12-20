## Creates an object with 4 functions:
# - set(): Stores a matrix
# - get(): Gets the stored matrix. NULL if nothing was stored
# - setinv(matrix): Stores a matrix (which is supposed to be the inverse of the stored matrix)
# - getinv(): Gets the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #Function to set the matrix value
  set <- function(y) {
    x <<- y
    #If the matrix changes, reset the inverse
    inv <<- NULL
  }
  #Function to get the matrix value
  get <- function() {
    x
  }
  #Function to set the inverse (which is calculated OUTSIDE this object)
  setinv <- function(solved){
    inv <<- solved
  } 
  #Function to get the inverse
  getinv <- function(){
    inv
  } 
  #Return the list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve retrieves the inv of a matrix stored in an object created with makeCacheMatrix()
## If it is already calculated retrieves the cached value, otherwise calculates and stores it
# x should be a matrix (we assume inverse exists)
# ... are parameters that will be passed to the solve function if needed.

cacheSolve <- function(x, ...) {
  #Get the inversed matrix
  m <- x$getinv()
  #Is it already calculated? NULL if not, a matrix otherwise
  if(!is.null(m)) {
    #If cached, return it
    message("This was cached")
    return(m)
  }else{
    #If not cached, calculate and store it
    message("This was NOT cached")
    #Get the matrix
    data <- x$get()
    #Solve it
    inv <- solve(data, ...) ## ... just in case we need to pass arguments to solve.
    #Store it
    x$setinv(inv)
    #Return the inverse matrix
    return(inv)
  }
}
