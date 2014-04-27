## makeCacheMatrix creates a special matrix Object.
## functions do 
## set value of the matrix
## get the value of the matrix
## set the value of inverse matrix
## get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inverse_x <- NULL
        set <- function(y) {
                x <<- y
                inverse_x <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) inverse_x <<-inverse
        getinverse <- function() inverse_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##cacheSolve function the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated and the matrix has not changed,
## will retrieve value from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_x <- x$getinverse()
        if (!is.null(inverse_x)) {
                message("getting cached inverse matrix")
                return(inverse_x)
        } else {
                inv_x <- solve(x$get())
                x$setinverse(inverse_x)
                return(inverse_x)
        }
}
