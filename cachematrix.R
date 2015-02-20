## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## I have created the two function
## There is no saved cace value in the furst run so the system will store the value in cache
## Second time you will get the value



makeCacheMatrix <- function(x = matrix()) {
        ## Define the variables and initialize them we are also updating them in global environmen using <<- operator
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        ## call the setinverse and the getinverse functions within the main function
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' check if 'x' is null to check if a value is not cashed
        ## if it it not then retun the statement
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        ## if the 'x' has no cached value then calculate it using the solve function and cache the value
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


## I have given some example of matrix here I used to test the fucntion

## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]    1    2
## [2,]    3    2

## There is no saved cace value in the furst run so the system will store the value in cache
## > cacheSolve(m)
##       [,1]  [,2]
## [1,] -0.50  0.50
## [2,]  0.75 -0.25

## Second time you will get the value
## > cacheSolve(m)
## getting cached data.
##       [,1]  [,2]
## [1,] -0.50  0.50
## [2,]  0.75 -0.25
## > 
