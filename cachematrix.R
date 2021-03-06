## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly (there are also alternatives to matrix inversion that 
## we will not discuss here). Your assignment is to write a pair of 
## functions that cache the inverse of a matrix.
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        xInv <- NULL
        PreviousX <- x
        # define set function
        set <- function(matrx) {
                x <<- matrx
                xInv <- NULL
        }
        
        # define get function
        get <- function() x
        
        # define  function to set the  inverse matrix
        setInv <- function(InvMatrix) xInv <- InvMatrix
        
        # define functin to return the inverse matrix
        getInv <- function() xInv
        
        # return the list of functions
        list(set=set, get=get, 
             setInv= setInv, 
             getInv = getInv)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        xInv <- x$getInv()
        # if already there and matrix x is same as last time 
        # return the inverse matrix from cache
        if(!is.null(xInv) && (x==PreviousX)) {
                message("Getting inverse matrix")
                return (xInv)
        }
        # if the matrix is NOT already there, get the matrix & compute its inverse
        xmatrix <- x$get()
        xInv <- solve(xmatrix) # then inverse the matrix
        x$setInv(xInv)
        ## Return the matrix that is the inverse of 'x'
        xInv
}
