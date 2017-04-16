## Programming Assignment 2: Lexical Scoping
## 2017.04.16

## This funcution cretes a cache of the inverst of a matrix. It will
## help save time when repeting the same computation.

makeCacheMatrix <- function(x = matrix()) {
    i = NULL
    set = function(y) {
        
        x <<- y # Here I use the `<<-` operator to assign a value to an object
        i <<- NULL
    }
    get = function() x
    setInv = function(inverse) i <<- inverse
    getInv = function() i
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## Calculates the inverse of the matrix created in the makeCacheMatrix while
## reusing a cached result (if there is one available)

cacheSolve <- function(x, ...) {
    i = x$getInv()
        
    # When the inverse is already cached and calculated...
    if (!is.null(i)){
        message("getting cached data")
        return(i)
        }
        
    # When the inverse does not already exist, we calculate the inverse here...
    m = x$get()
    i = solve(m, ...)
    
    x$setInv(i)
        
    return(i)
}
