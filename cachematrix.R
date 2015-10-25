## The two functions mentioned below work together to come up with a cached 
## implementation of Matrix Inversion. The main benefit here it that by caching
## we save on the constant inversion of matrix operation which is otherwise a 
## costly operation. The two functions are also a guideline for creating
## cached implementation of other operations in R



## the makeCacheMatrix provides functions/implementation that are required
## to create a cached version of a Matrix.
## Inv is initially set to NULL. Also the set method sets inverse to NULL again so that it has to be re calculated once set through function
## The idea is to pass this Matric implementation to cache Solve so that the inverse can be cached
## Also notive the inv <<- NULL. This is to make sure inv is set to NULL in the parent environment

makeCacheMatrix <- function(x = matrix()) {
 
  inv <- NULL
  set <- function(y) {
    
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(invers) inv <<- invers
  getInverse <- function() inv
  
  list(set=set, get=get, setInverse = setInverse,getInverse = getInverse)

}


## This method takes a cacheMatrix as input
## Checks if the matrix inverse is already calculated and the matrix itself has not changed the cached inverse is returned
## Else an inverse is calculated and cached by invoking setInverse method of cacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		inv <- x$getInverse()
  if (!is.null(inv)){
    
    y <- x$get()
   
    if (is.matrix(x$get()) && is.matrix(y) && dim(x$get()) == dim(y) && all(x$get() == y)){
      message("getting cached data")
      return(inv)
    }
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
