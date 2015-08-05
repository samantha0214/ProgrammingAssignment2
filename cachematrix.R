#The following two function are used to find the inverse of a matrix.

#makeMatrix creates a list containing a function to :

#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of inverse of the matrix
#4.get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
         x <<- y
         i <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) i <<- inverse
     getinv <- function() i
     list(set=set, get=get, setinverse=setinv, getinverse=getinv)
 }

#The following function returns the inverse of the matrix.
#If the inverse has already been computed,it gets the result and 
#skip the computation.

cacheSolve <- function(x, ...) {
     i <- x$geti()
     if(!is.null(i)) {
         message("getting cached data.")
         return(i)
     }
     data <- x$get()
     i <- solve(data)
     x$seti(i)
     i
 }

##
#Sample:
# x = cbind(c(1, 1/3), c(1/3, 1))
# m = makeCacheMatrix(x)
# m$get()
#         [,1]      [,2]
# [1,] 1.0000000 0.3333333
# [2,] 0.3333333 1.0000000
#
# No cache in the first run
# cacheSolve(m)
#        [,1]   [,2]
# [1,]  1.125 -0.375
# [2,] -0.375  1.125
#
# Second run
# cacheSolve(m)
# getting cached data.
#        [,1]   [,2]
# [1,]  1.125 -0.375
# [2,] -0.375  1.125

