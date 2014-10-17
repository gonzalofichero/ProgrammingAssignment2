# Pair of functions to store in cache the inverse of a matrix
# Basicly is the example given in the assigment, and changing the mean by the inverse...


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
    
    # Initialize the inverse property
    i <- NULL
    
    # Method to set the matrix
    # Asigning with <<- because "m" y i" are from other environment
    set <- function( matrix ) {
        m <<- matrix
        i <<- NULL
    }
    
    # Method the get the matrix
    get <- function() {
        ## Return the matrix
        m
    }
    
    # Method to set the inverse of the matrix
    #  Asigning with <<- because i" is from other environment
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    # Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }
    
    # Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# Computes what is returned by makeCacheMatrix.R
# If the inverse has already been calculated, then the function
# should only retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    # Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    # Just return the inverse if its already set
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    
    # Get the matrix from our object
    data <- x$get()
    
    # Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data
    
    # Set the inverse to the object
    x$setInverse(m)
    
    # Return the matrix
    m
}