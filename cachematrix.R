## Put comments here that give an overall description of what your
## functions do

## Encapsulates the inverse of the given matrix using caching.

makeCacheMatrix <- function( m = matrix() ) {

	## Initialize the inverse property
    inv <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            inv <<- NULL
    }

    ## Method the get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        inv
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## Produces the inverse of the matrix being returned by the previous function (makeCacheMatrix).
## If the inverse is already computed, it will retuen the already computed value.

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    A <- x$getInverse()
## Just return the inverse if its already set
    if( !is.null(A) ) {
            message("getting cached data")
            return(A)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    A <- solve(data)

    ## Set the inverse to the object
    x$setInverse(A)

    ## Return the matrix
    A
}


#example
#as<-matrix(1:4,2,2)
#second<-makeCacheMatrix(as)
#print(second$inv)
#cacheSolve(second)
#output
#NULL
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
