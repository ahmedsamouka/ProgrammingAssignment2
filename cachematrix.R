## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL ##this is where the result of inversion is stored
        set <- function(y) {
                x <<- y
                invert <<- NULL ## it also initialises invert to null
        }
        get <- function() x ## return the input matrix
        setInverse <- function(inverse) invert <<- inverse ##set the inversed matrix
        getInverse <- function() invert ## return the inversed matrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invert <- x$getInverse() ## get the inversed matrix from object x
        if (!is.null(invert)) { ## if the inversion result is there
                message("getting cached data")
                return(invert) ## return the calculated inversion
        }
        mat <- x$get() ##if not, we do x$get to get the matrix object
        invert <- solve(mat, ...)  ##we solve it
        x$setInverse(invert) ##we then set it to the object
        invert ##return the solved result
}
