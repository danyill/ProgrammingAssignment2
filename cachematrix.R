
# * makeCacheMatrix creates a matrix and sets the value of it in a cache
# 	It provides the following methods to access the cache:
# 		- set: for storing a value to calculate the inverse of
# 		- get: for retrieving a value to calculate the inverse of
#		- setinverse: for storing the calculated inverse value
# 		- getinverse: for retrieving the calculated inverse value
#
# * cacheSolve does the following:
# 		- It retrieves the inverse from the cache if it was already 
# 		  calculated. 
# 		- If not previously calculated, it calculates the inverse and 
# 		  stores in the cache provided by makeCacheMatrix.

# makeCacheMatrix reads a matrix input, x and creates functions to set 
# and retrieve the value x, as well as to set and retrieve the inverse 
# using a separate environment.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
      
        setinverse <- function(inverse) m <<- inverse
        
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

# cacheSolve uses the makeCacheMatrix function to determine if an 
#   inverse has already been calculated. If it has, it retrieves the 
#   value and returns it, otherwise it calculates the inverse, stores
#   it in the cache and returns the value to the user.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("Getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


