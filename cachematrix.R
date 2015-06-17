## Functions "makeCacheMatrix" and "cacheSolve" are used to invert matrix  
## and to put result into cahe for further use if needed. This prevents from 
## repeating of costly inverse matrix computations if the matrix was not changed.

## Example of calling sequence:
## my_matrix <- matrix(sample(1:25), 5, 5) # initiate some random 5x5 numeric matrix
## my_matrix_object <- makeCacheMatrix(my_matrix) # prepare special matrix object
## cacheSolve(my_matrix_object) # calculate the inverse and cache it
## cacheSolve(my_matrix_object) # second call, inverted matrix is retrieved from cache

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse. Function returns the list of functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverted matrix
## get the value of the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    
    inverted_matrix <- NULL
    set <- function(y) {
        x <<- y
        inverted_matrix <<- NULL
    }
    get <- function() x
    setinvert <- function(inverted_matrix_to_set) 
                    inverted_matrix <<- inverted_matrix_to_set
    getinvert <- function() inverted_matrix
    
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)

}


## cacheSolve: this function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function. If the inverse has already been 
## calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    inverted_matrix <- x$getinvert()
    
    if (!is.null(inverted_matrix)){
        message("getting cached matrix inverse")
        return(inverted_matrix)
    }
    data2inverse <- x$get()
    inverted_matrix <- solve(data2inverse, ...)
    x$setinvert(inverted_matrix)
    inverted_matrix
    
}
