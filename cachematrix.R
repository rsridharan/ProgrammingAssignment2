## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function makeCashMatrix creats a special "vector", which is really a list
## containg a function set of the matrix,x, get the matrix, setinverse (of 
## the matrix) and getinverse (of the matrix).

makeCacheMatrix <- function(x = matrix()) { ## x is data
    mtxinverse <- NULL
    setmatrix <- function(y){
                x <<- y
                mtxinverse <<- solve(x) ## mtxinverse is the inverse of x
    }
    getmatrix <- function() x
    xinverse <- solve(x)
    setinverse <- function(xinverse) mtxinverse <<- xinverse
    getinverse <- function() xinverse
    list( setmatrix = setmatrix, getmatrix = getmatrix,
          setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

## The following function determines the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been determined. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it determines the inverse of
## the data and sets the inverse in the cache via the `setinverse` function

cacheSolve <- function(x, ...) {
    mtxinverse <- x$getinverse()
    if(!is.null(mtxinverse)){
        message("getting chached data")
        return(mtxinverse)
    }
      mtx <- x$getmatrix()
      mtxinverse <- solve(x)
      x$setinverse(mtxinverse)
      mtxinverse
    ## Return a matrix that is the inverse of 'x'
}
