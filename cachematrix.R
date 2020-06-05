# -----------------------------------------------------------------
# Student: Alejandro Alonso Salas Vargas
# Assignment 2 (Lexical Scoping): Caching the Inverse of a Matrix
# R Programming
# -----------------------------------------------------------------


## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  if(det(x)==0){stop("Error: The matrix is not invertible")}
  inv <- NULL
  set <- function(y) {x <<- y; inv <<- NULL}
  get <- function(){x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  
  return(list(set = set, get = get,
              setinverse = setinverse, 
              getinverse = getinverse))
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("Inverse matrix obtained from the cache.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  message("Inverse matrix computed for the first time.")
  return(inv)
}

# The matrix is defined
M <- matrix(c(1,0,2,2,-3,1,-1,2,5),3,3)

invM <- makeCacheMatrix(M) #Caching the inverse of a matrix.
cacheSolve(invM) #Inverse matrix computed for the first time.
cacheSolve(invM) #Inverse matrix obtained from the cache.
