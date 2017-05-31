makeCacheMatrix <- function(X = matrix()) {
        invmatrix <- NULL
        set <- function(y) {
                X <<- y
                inv <<- NULL
        }
        get <- function() X
        setinverse <- function(inverse) invmatrix <<- inverse
        getinverse <- function() invmatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)	
}

cacheSolve <- function(X, ...) {
        invmatrix <- X$getinverse()
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(invmatrix)
        }
        
        data <- X$get()
        invmatrix <- solve(data, ...)
        X$setinverse(invmatrix)
        invmatrix 
}