## The purpose of this script is to create a matrices and cache them. 
## It checks if the inverse of it is computed and cached before re-calculating.. 
## the inverse

## This function creates a matirx with functions to create and cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) m <<-inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of the matrix once created above. 
## If avaialble it retrieves the matrix inverse.
## If not available it computes and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)) {
                message("get cached inv matrix")
                return(m)
        } else {
                m <- solve(x$get())   # calculate the inverse
                x$setinverse(m)
                return(m)
        }
}
