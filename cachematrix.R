## Put comments here that give an overall description of what your
## functions do
#
## Coursera R Programming, Programming Assignment 2: Lexical Scoping
## Caching the inverse of a matrix
## Tomas E. Tecce, March 2015
#
## These two functions cache the inverse of a matrix, so that if the matrix
## doesn't change we retrieve the inverse from the cache instead of calculating
## the inverse all over again, which is usually time-consuming.
## Both functions are simple modifications of the functions makeVector and
## cachemean provided as examples in the Assignment.

## Write a short comment describing this function
#
## Function makeCacheMatrix transforms a standard R matrix into an object which
## stores the matrix and its inverse.
## For the purposes of this exercise I assume that the matrix passed to this
## function is always invertible - thus I haven't added any checks to verify
## whether x is indeed a square matrix, if it's numeric, etc. R function solve()
## returns the inverse of a square matrix.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set=set, get=get,
         setinv=setinv,
         getinv=getinv)
}


## Write a short comment describing this function
#
## This function computes the inverse of a matrix object created by function
## makeCacheMatrix. If the matrix doesn't change, calling this function again
## will return the cached value of the inverse instead of computing it
## repeatedly.
## deparse(substitute()) is added to the message to also print out the name of
## the matrix.
## I'm assuming that a valid object is always passed to the function, so I'm not
## adding any checks for errors
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message(paste("Getting cached inverse data for",
                      deparse(substitute(x))))
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}