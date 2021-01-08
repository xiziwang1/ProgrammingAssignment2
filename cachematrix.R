## This function is to cashe the inverse of a matrix


## The first function, makeCacheMatrix is to 
## create a special "matrix" object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                   m <- NULL
                   
                   set <- function(y){
                     x <<- y
                     m <<- NULL
                   }
                   get <- function() x
                   setinverse <- function(solve) m <<- solve
                   getinverse <- function() m
                   
                   list(set=set, get=get,
                        setinverse=setinverse,
                        getinverse=getinverse)

}


## This function computes the inverse of the special matrix
## created from the previous function. This will retrieve the inverse matrix if
## the inverse matrix was correctly calculated.

cacheSolve <- function(x, ...) {
              m <- x$getinverse()
              if(!is.null(m)){
                      message("getting cached data")
                      return(m)
              }
              data <- x$get()
              m <- solve(data, ...)
              x$setinverse(m)
              m
}
