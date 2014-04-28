## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a special matrix that returns the matrix in get / stores in set, and can getInverse and cache it 

makeCacheMatrix <- function(x = matrix()) {

 inv <- NULL
  set <- function(y,...) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv<- x$getInverse()        #query the x cache to see if inverse isin the cache
  
        if(!is.null(inv)) {          #if there is a cache
                message("getting cached data") 
        return(inv)                  #just return the cache, no computation needed
        }
        
        data <- x$get()              #if there's no cache
        inv <- solve(data, ...)      #actually compute the inverse here
        x$setInverse(inv)            #save the result back to x's cache
        inv  
}
