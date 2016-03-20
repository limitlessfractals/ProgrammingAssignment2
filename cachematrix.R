## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## cacheSolve calculates the inverse matrix cached by makeCacheMatrix
## If the inverse was already solve, it retrieves the inverse directly from cache

## Caching saves time because it avoids solving the same matrix inversion over and over again


## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix", which is really a list
## that contains functions to 
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setInv = function(inverse) inv <<- inverse
        getInv = function() inv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
        
}


## Solves the matrix stored in cache or retrieve directly from cache if already solved

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv = x$getInv()
        
        # skip calculation and retrieve from cache if already solved
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        # calculate inverse then store to cache
        m = x$get()
        inv = solve(m, ...)
        x$setInv(inv)
        inv
}
