##create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
##set value of matrix
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
##get value of matrix
        get <- function() x
##set value of inversed matrix
        setinverse <- function(inverse) inv <<- inverse
##get value of inversed matrix
        getinverse <- function() inv
        list(set = set, get = get, 
                setinverse = setinverse
                getinverse = getinverse)
}

##compute the inverse of the special matrix returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
##return inverse of matrix
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
##solve inverse of matrix
        data <- x$get()
        inv <- solve(data, ...)
##set value of inversed matrix in cache
        x$setinverse(inv)
        inv
}
