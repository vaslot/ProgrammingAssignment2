## Matrix inversion is usually a costly computation, so there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. The following routines facilitate computing inverse of a square
## matrix and cacheing it for later retrieval. 
##
## Programming model for using the routines
## -----------------------------------------
## Anyone wanting to use these routines would first call 
## makeCacheMatrix(your_matrix), which will return a special opaque 
## object initialized with your_matrix back. Then the user can
## call cacheSolve(object) which will return the inverse of your_matrix.
## 

## makeCacheMatrix()
## -----------------
## This routine takes your matrix as an input and returns an opaque object
## initialized with your matrix as well as other methods back. The object
## can later be passed into cacheSolve() routine in order to compute the
## inverse of a matrix.
##
## This routine assumes that the matrix is a quare invertible one. It does not
## confirm this fact. If a non-invertible matrix is passed in, the results
## are undefined.
makeCacheMatrix <- function(x = matrix()) 
{
        inv_x <- NULL
        set <- function(y)
        {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inv_x <<- inv
        getInverse <- function() inv_x
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve()
## ------------
## This routine computes the inverse of a square invertible matrix. It takes
## a special object previously returned by makeCacheMatrix() as the input
## and computes the inverse of the matrix inside the object.
##
## This routine is efficient, because it checks if an inverse already exists
## in the cache. If so, it returns the cached copy. If not, it recomputes it.
##
## This routine assumes that the matrix is a quare invertible one. It does not
## confirm this fact. If a non-invertible matrix is passed in, the results
## are undefined. Also, if the cached answer is present, it returns it without
## checking if the matrix has changed. In other words, it assumes that the
## matrix inside the object 'x' has not changed since creation.
cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getInverse()
        if (!is.null(inv_x))
        {
                message("getting cached inverse")
                return(inv_x)
        }
        m <- x$get()
        inv_x <- solve(m)
        x$setInverse(inv_x)
        m
}
