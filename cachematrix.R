##1 makeCacheMatric: function that creates a special matric object that can cache its inverse

makeCacheMatrix <- function(x = maxtrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  get <- function() {x} #function to get matrix
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() {inv}  #function to obtain the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##2 cacheSolve: function that computes the inverse of the special matrix returned by MakeCacheMatrix. If inverse has already been calculated and matrix not changed, then the cacheSolve shoul retrieve the inverse from the cache. 

#Computing the inverse of a square matrix can be done using hte solve function if X is square then solve(x) returns its inverse, assume that the matrix supplied is always invertible. 
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {             #Checks to see whether inv is NULL
    message("getting cached data")
    return(inv)           #If inv is not NULL then value returned
  }
  mat <- x$get()              #If inv is Null then inverse calculated
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
