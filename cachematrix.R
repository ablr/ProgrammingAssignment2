## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Includes set, get, setinverse and getinverse functions for the matrix

makeCacheMatrix <- function(x = matrix()) {
   i = matrix()
   i <- NULL
   set <- function(y = matrix()) {
      x <<- y
      i <<- NULL
   }
   get <- function() x
   setinverse <- function(inv) i <<- inv
   getinverse <- function() i
   list(set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   i <- x$getinverse()
   if(!is.null(i)) {
      message("getting cached inverse")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
}

## Create a Matrix
##>x <- matrix(1:4,2,2)
##> x
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
## Make the Cache Matrix with x
##> m = makeCacheMatrix(x)
## Check m has been created
##> m$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
## Run the inverse and get the inverse 
##> cacheSolve(m)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
## Run the inverse again and get the inverse from cache
##> cacheSolve(m)
##getting cached inverse
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5