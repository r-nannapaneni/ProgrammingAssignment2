## These functions try to cache an inverse of matrix which can be used instead of recomputing it,
## until the contents of the matrix rmain unchanged.

## The first function creates a list of functions to set the matrix, get the matrix, set the inverse of 
## the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        # returns a list
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will compute the inverse of the function created with the above function. 
## It will first check and fetch the inverse, if the inverse is already computed 
## else it will calculate the inverse and will store the value in cache 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setinverse(inv)
        inv
        ## Returns a matrix that is the inverse of 'x'
}
