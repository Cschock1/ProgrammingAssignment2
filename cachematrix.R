####makeCacheMatrix####
###Stores a numeric vector (matrix) and:
###sets the value of the vector, 
###gets the value of the vector, 
###sets the value of the inverse, and 
###gets the value of the inverse.
###
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) i <<- inverse
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}
Matrix1 <- makeCacheMatrix(matrix(c(4,4,2,4), 2, 2))
Matrix1$get()

#########cacheSolve##########
###Solves for the inverse of the matrix created with makeCacheMatrix. 
###If the inverse has previously been calculated and cached, it retrieves the inverse.
### Otherwise, it calcualtes the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)) {
    return(i)
  }
  MatrixInverse <- x$get()
  i <- solve(MatrixInverse, ...)
  x$setInv(i)
  i
}
cacheSolve(Matrix1)
