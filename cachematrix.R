#Matrix inversion is usually a costly computation and there may 
#be some benefit to caching the inverse of a matrix rather than compute it repeatedly
#Two following functions are supposed to do the caching


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                       
        set <- function(y) {    # setting the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x     #getting the value of the matrix
        setsolve <- function(solve) m <<- solve         #setting the value of inverse of the matrix
        getsolve <- function() m                #getting the value of inverse of the matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# The function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) { #checking existence of already calculated matrix
                message("getting cached data")
                return(m) #if TRUE - return matrix
        }
        data <- x$get()
        m <- solve(data, ...) #if FALSE - calculate inverse
        x$setsolve(m)
        m
}
