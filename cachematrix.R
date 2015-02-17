####################################################
# The following functions provide an environment for 
# creating, caching and retrieving the inverse of
# a square matrix. 
# This speeds up retrieval of the inverted matrix after
# the first time, by returning the cached inverse,
# rather than executing the matrix inversion algorithm
# every time.
#
# Usage:
#   initialize using:
#       myFunctionList <- makeCacheMatrix (myMatrix),
#   then call for each access to the the inverted matrix: 
#       cacheSolve(myFunctionList)
#       
####################################################

# makeCacheMatrix creates and returns the list of functions 
# to operate on the cache of the inverse matrix. 
#
# The internal environment includes the list of functions, 
# the original matrix, the inverse of the matrix.
#
# Usage:  myFunctionList <- makeCacheMatrix (myMatrix)
#         where
#           myFunctionList is the returned list of functions, 
#           myMatrix is the original matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    setMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getMatrix <- function() x
    setInvertedMatrix <- function(mat) m <<- mat
    getInvertedMatrix <- function() m
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setInvertedMatrix = setInvertedMatrix,
         getInvertedMatrix = getInvertedMatrix)
    
}

# cacheSolve retrieves the inverted matrix from the cache.
# If the cache is empty, the original matrix is inverted,
# the result is stored in the cache, as well as returned.
#
# Usage:  myMatrixInverse <- cacheSolve(myFunctionList)
#         where
#           myMatrixInverse is the inverse of the original matrix,
#           myFunctionList is the list of functions that are
#             associated with the original matrix after 
#             makeCacheMatrix() has been called, 
#         

cacheSolve <- function(x, ...) {
    m <- x$getInvertedMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    m <- solve(x$getMatrix())
    x$setInvertedMatrix(m)

    return(m)
}

