## The overall function is to catch the inverse of a matrix rather
## than computing repeatedly. 

## This function creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #If there is no matrix then it is supposed as NULL
    set <- function (y) { #setting a function for specific matrix
        x <<- y
        inv <<- NULL
    }
    get <- function()x #get the value of the matrix
    setinverse <- function(inverse) inv <<- inverse #set the value of inverse of the matrix
    getinverse <- function() inv #get the value of inverse of the matrix
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)

}


## The cachesolve function computes the inverse of the special matrix
## returned by makeCacheMatrix
#It first checks to see if the inverse has already been computed. 
#It the inverse has already been calculated, then this function retrieves the
#inverse from the cache, if not it computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) { #to see if the inverse has already been calculated
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data) #calculates inverse of the matrix
    x$setinverse(inv) #set inverse of the matrix
    inv
}
