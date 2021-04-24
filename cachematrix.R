## Caching the inverse of a 2*2 matrix

## Creating special matrix object that creates the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 

## Setting matrix value
  set <- function(y) {
    x <<- y ## Closure environment
    inv <<- NULL
  }

## To get the value of matrix
  get <- function() {x}
  
## For setting and getting value of inverse of given matrix
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##  To compute the inverse of the special "matrix" created by 'makeCacheMatrix' 
cacheSolve <- function(x, ...) {
  inv <- x$getinverse() ## Check if inverse is already computed
  if(!is.null(inv)) {
    message("getting cached data") ## For retrieving inverse from cache
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Making a 2*2 matrix to check our formula
pmatrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))

## Get the matrix
pmatrix$get()

## Try to get inverse of the matrix
pmatrix$getinverse()

## Will return the inverse from Cache
cacheSolve(pmatrix)
cacheSolve(pmatrix)

## Solves for the value of inverse from cache
pmatrix$getinverse()
