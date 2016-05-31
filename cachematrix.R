## Below are two functions that are used to create a special
## object that stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cacheSolve should retrieve the inverse from the cache.
## Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the solve function.

cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data,...)
        x$setinverse(inver)
        inver
        ## Return a matrix that is the inverse of 'x'
}
