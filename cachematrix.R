## cachematrix.R
##
## Version      Date            By
##      1.0     2017-02-25      John Carter
##
## Description:
## This module contains functions to perform matrix invesion, cache the result and retrieve the cached 
## result at a later time.


## makeCacheMatrix:  build set of required functions and return list to parent environment
makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(y) 
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function (solve) m <<- solve
        getInverse <- function() m
        list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve:  return cached inverse of matrix if avaiable, otherwise solve inversion, cache and return
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if (!is.null(m)) 
        {
                ## return cached version
                return (m)
        }
        ## else compute inverse, cache and return
        data <- x$get()
        m <- solve (data, ...)
        x$setInverse (m)
        m
}
