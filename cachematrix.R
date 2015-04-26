#This is a code file for Programming Assignment #2
##Written by Darcy Vavrek due April 26, 2015

###This second programming assignment will require you to write an
###R function that is able to cache potentially time-consuming computations. 
###For example, taking the mean of a numeric vector is typically a 
###fast operation. 
###However, for a very long vector, it may take too long to compute the 
###mean, especially if it has to be computed repeatedly (e.g. in a loop). 
###If the contents of a vector are not changing, it may make sense to cache 
###the value of the mean so that when we need it again, it can be looked up 
###in the cache rather than recomputed. 
###In this Programming Assignment you will take advantage of the scoping 
###rules of the R language and how they can be manipulated to preserve 
###state inside of an R object.

####Caching the Inverse of a Matrix 
#####<Notation throughout entire document is simply a modification of 
#####the original vector example>

####Note: The <<- operator is used to assign a value to an object in an 
####environment that is different from the current environment. 
####Below are two functions that are used to create a special object 
####that stores a matrix and caches its inverse.

####The first function, makeCacheMatrix creates a special "matrix", 
####which is really a list containing a function to

#####set the value of the matrix
#####get the value of the matrix
#####set the value of the inverse
#####get the value of the inverse

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

####The following function calculates the inverse of the special 
####"matrix" created with the above function. 
####However, it first checks to see if the inverse has already been calculated. 
####If so, it gets the inverse from the cache and skips the computation. 
####Otherwise, it calculates the inverse of the matrix and sets the value of 
####the inverse in the cache via the setinverse function.

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


##Test results:
#> c<-rbind(c(1, -1/4), c(-1/4, 1))       #A test matrix 
#> is.matrix(c)                           #This really is a matrix
#[1] TRUE
#> solve(c)                               #This matrix really has an inverse
#[,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667
#> makeCacheMatrix(c)                     #output of makeCacheMatrix
#$set
#function (y) 
#{
#       x <<- y
#       m <<- NULL
#}
#<environment: 0x0000000008d934b8>
#       
#       $get
#function () 
#       x
#<environment: 0x0000000008d934b8>
#       
#       $setinverse
#function (solve) 
#       m <<- solve
#<environment: 0x0000000008d934b8>
#       
#       $getinverse
#function () 
#       m
#<environment: 0x0000000008d934b8>
#       
#> test<-makeCacheMatrix(c)               #save output to a variable
#> solve(c)                               #inverse for comparison
#[,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667
#> cacheSolve(test)                       #Call the inverse in the cache
#[,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667
