## Put comments here that give an overall description of what your
## functions do

## This pair of functions enables you to cache the inverse of a
## matrix, due to enhance processing time of algorithms that
## calculates the inverse matrix several times

## Write a short comment describing this function

## Creates a class variable that caches the matrix and its inverse
## The first time you set the matrix, its inverse won't be available
## In order to calculate the inverse matrix, you should call for
## "cacheSolve" function and provide the makeCacheMatrix
## object as "x" argument.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function()
        x
    setInverse <- function(inverse)
        i <<- inverse
    getInverse <- function()
        i
    ## Nominate the elements of the list so you can call the element
    ## by using it's name rather than the number
    list(
        set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
    
}


## Write a short comment describing this function

## Calculates the matrix inverse of matrix by makeCacheMatrix "x"
## parameter and caches it at the into the same "x" parameter
## using the "setInverse" member function.
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached inverse matrix")
    }else{
        message("calculating the inverse matrix")
        mtx <- x$get()
        i <- solve(mtx,...)
        x$setInverse(i)
    }
    i
}
