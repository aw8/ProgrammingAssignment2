## R Programming: Assignment 2
## makeCacheMatrix and cacheSolve are functions to create/access an R object
## that can store a matrix and cache its inverse.


## makeCacheMatrix creates an R object that stores both a matrix x,
## and can cache its inverse.
## This object also contains functions to retrieve and update both the matrix
## and its cached inverse.
makeCacheMatrix <- function(x = matrix()) {
    #initialise inverse of matrix to NULL (not calculated yet)
    inv <- NULL
    
    #set function: allows stored matrix to be changed
    set <- function(y) {
        # change stored matrix
        x <<- y
        # clear cached inverse (as stored matrix has changed)
        m <<- NULL
    }
    
    #get function: retrieves matrix
    get <- function() x
    
    #setinv function: stores calculated inverse matrix for future use
    setinv <- function(inverse) inv <<- inverse
    
    #getinv function: retrieves cached version of calculated matrix inverse, or
    # NULL (if inverse has yet to be calculated)
    getinv <- function() inv
    
    #create a list containing all 4 functions for use by cacheSolve
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve returns a matrix that is the inverse of that stored in x:
## the function either calculates the inverse, caches it and returns it, or
## retrieves the cached inverse and returns it.
cacheSolve <- function(x, ...) {
    #retrieve data from location of cached inverse
    inv <- x$getinv()
    
    #if inverse has been calculated previously (cached data is not NULL)...
    if(!is.null(inv)) {
        message("retrieving cached inverse matrix")
        # ...return cached inverse matrix
        return(inv)
    }
    
    #if inverse has not been calculated previously (cached data is NULL),
    #retrieve original matrix
    data <- x$get()
    #calculate matrix inverse
    inv <- solve(data, ...)
    #cache matrix inverse
    x$setinv(inv)
    #return matrix inverse
    inv
}
