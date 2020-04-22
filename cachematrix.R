## makeCacheMatrix creates a special "matrix" object with it's own set of functions
## cacheSolve will solve for makeCacheMatrix's inverse, if it's already calculated it will
## retrieve it from cache otherwise it will calculate it and return result.

## This function creates a special "matrix" object that can cache its inverse
## it returns a list containing functions to: 
## set, get, setInverse, getInverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## for saving or cache of inverse of matrix result
        set <- function(y) {
                x <<- y ##reset given matrix to whatever 'y' matrix is
                i <<- NULL ##reset inverse matrix result to null
        }
        get <- function() {
                x ##returning the given matrix 'x'
        }
        setInverse <- function(inv) {
                i <<- inv ##sets the inverse matrix to given 'i' matrix
        }
        getInverse <- function() {
                i ##returns the inverse matrix
        }
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ##returns a list with all the functions
}


## This function  calculates the inverse of the special "matrix"
## it will first check to see if inverse already calculated
## if so it will get it from cache and return result. Otherwise
## calculates the inverse and returns result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse() ##get the inverse from the passed in 'x' matrix
        if(!is.null(i)) { ##see inverse matrix 'i' is not null then return it, that's the desired solution
                message("getting cached data")
                return(i)
        }
        data <- x$get() ##if no cahced inverse then calculate it and return the results
        i <- solve(data)
        x$setInverse(i)
        i
}
