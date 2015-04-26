## Following functions cache matrix and its inverse, 
## also compute matrix inverse, in case if inverse wasn't previously found. 


## Creates a special 'matrix' object. Contains a list of functions:
##      set - cache a matrix
##      get - get a matrix
##      setinverse - cache the inverse of this matrix
##      getinverse - get the inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {
        c<-NULL
        set <- function(y=matrix()) {
                c <<-NULL
                x <<- y
        }
        get <- function() x
        setinverse <- function(inv) c <<- inv
        getinverse <- function() c
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special 'matrix' returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the function retrieve the inverse from the cache.
## Returns a matrix.

cacheSolve <- function(x, ...) {
        c <- x$getinverse()
        if(!is.null(c)) {
                message("getting cached data")
                return(c)
        }
        data <- x$get()
        c <- solve(data, ...)
        x$setinverse(c)
        c
}
