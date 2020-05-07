## makeCacheMatrix creates a special "matrix" object that can cache its inverse 
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 

## makeCacheMatrix first initializes the objects 'x' and 'm'. Then it defines 4 functions for those objects - 2 getters and 2 setters.
## 'set' uses a new argument, y, to reset object x in parent environment. It also resets object m to Null in parent environment (through <<-)
## Then, with potentially newly defined values for the objects in the parent environment, 'get' simply retrieves value of object x
## 'setinverse' determines m in the parent function will be anonymous function named 'inverse' (source: x$setinverse(m) in cacheSolve)
## 'getinverse' gets the new value (or original value) of m located in the parent environment
## Finally, output is a list containing those 4 functions and naming them so we can easily retrieve their data in other functions.

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) m <<- inverse 
                getinverse <- function() m
                list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve first creates a function with argument 'x' and ellipsis so we can pass additional arguments to the function.
## It then pulls the inverse matrix from getinverse (source: makeCacheMatrix) IF it is not null and the message and inverse matrix will be returned  
## If it is null (i.e. if we changed the matrix since last time cacheSolve was run), we are retrieving the new matrix from 'get'
## and calculating its inverse. We then put the inverse we just calculated back into makeCacheMatrix, specifically in setinverse function, 
## so we can retrieve its value later if matrix doesn't change. Finally, set output to the actual inverse of the matrix.

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
