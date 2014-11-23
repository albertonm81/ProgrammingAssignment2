## A pair of functions that cache the inverse of a matrix


## To make a matrix to cache the inverse
makeCacheMatrix <- function( m = matrix() ) {

	## We initialize the inverse property
    inv <- NULL

    ## To set the matrix
    set <- function( matrix ) {
            mat <<- matrix
            inv <<- NULL
    }

    ## To get the matrix
    get <- function() {
    	## Return the matrix
    	mat
    }

    ## To set the inverse matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }

    ## To get the inverse matrix
    getInverse <- function() {
        ## Return the inverse property
        inv
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    mat <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(mat) ) {
            message("getting cached data")
            return(mat)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    mat <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(mat)

    ## Return the matrix
    mat
}