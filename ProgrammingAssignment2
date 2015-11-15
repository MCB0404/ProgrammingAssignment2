## Two functions here. The first makeCacheMatrix will make a special matrix object to cache its inverse
## The second cacheSolve will either 1) pull the cached inverse, or 2) solve for the inverse and cache it

#Here I just create a simple matrix, print it, and calculate its inverse
mat<-matrix(1:4,2,2)
mat
solve(mat)

## First function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Will retrieve the inverse of x that has been cached or will calculate the inverse with solve function and setInverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invF <- x$getinverse()
  if(!is.null(invF)) {
    message("getting cached data") # If this statement appears in output, then you know it pulled the cached inverse
    return(invF)
  }
  data <- x$get()
  invF <- solve(data)
  x$setinverse(invF)
  invF
}
mat
k<-makeCacheMatrix(mat)
k$get()
cacheSolve(k) #First time it solves
cacheSolve(k) #Second time it says "getting cached data" in output of console
