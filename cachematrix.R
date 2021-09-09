## Put comments here that give an overall description of what your
## functions do



## Write a short comment describing this function
#The first function, makeCacheMatrix creates a special "matrix" object
#that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
m.local <- makeCacheMatrix.object$solve()
if(!is.null(m.local)) {
         message("getting cached data")
         return(m.local)
 }
 data <- makeCacheMatrix.object$get()
 m.local.calculated <- solve(data, ...)
 makeCacheMatrix.object$solve(m.local.calculated)
 m.local.calculated # return the inverse

}


## Write a short comment describing this function

#This function computes the inverse of the special "matrix" returned by
#makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should retrieve
#the inverse from the cache.
#Computing the inverse of a square matrix can be done with the
#solve function in R. For example, if X is a square invertible matrix,
#then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        #check cache first and return that inverse if cached

        if(!is.null(m.local.calculated)) {
                message("getting cached data")
                return(m.local.calculated)
        }

        #otherwise, calculate and return inverse

        return(solve(x))


        m <- x$getm()

data <- x$get()
m <- solve(data, ...)
x$setinverse(m)
m


}
