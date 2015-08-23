#This is a code file for Programming Assignment #2
##...really, this is just a modified version of the cacheMean example...
##
###Caching the Inverse of a Matrix 
###
###The first function, makeCacheMatrix, creates a special "matrix." 
###This is really a list containing a function to:
###*get the value of the matrix
###*get the value of the matrix
###*set the value of the inverse
###*get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setinverse <- function(solve) m <<- solve
       getinverse <- function() m
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}

###The second function, cacheSolve calculates, the inverse of the special 
###"matrix" created with the first function. 
###It checks to see if the inverse has already been calculated. 
###If so, it gets the inverse from the cache and skips the computation. 
###Otherwise, it calculates the inverse of the matrix and sets the value of 
###the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)) {
              message("getting cached data")
              return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setinverse(m)
       m
}
