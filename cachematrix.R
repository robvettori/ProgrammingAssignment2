###############################################################################
# makeCacheMatrix(x)
#
# Description:
# This function creates a special "matrix" object that can cache its inverse.
# It returns the "matrix" as a list
# 
# Arguments:
# x - A matrix
# 
# Functions:
# set(y) - Takes in a matrix 'y' and sets the "matrix" to 'y'
# get() - Returns the current "matrix"
# setInv(mInv) - Caches the inverse of the "matrix", 'mInv' 
# getInv() - Returns the current "matrix" inverse
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(y) {             
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(mInv) inv <<- mInv
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

###############################################################################
# cacheSolve(x)
#
# Description:
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cacheSolve retrieves the inverse from the
# cache.
#
# Arguments:
# x - a special matrix created by makeCacheMatrix
#
# Returns:
# Matrix inverse
###############################################################################
cacheSolve <- function(x) {
        # Retrieve the current matrix inverse
        inv <- x$getInv()
        
        # Test to see if the inverse exists
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Retrieve matrix and calculate the inverse
        inv <- solve(x$get())
        
        # Set the matrix inverse
        x$setInv(inv)
        inv
}
