## These functions create a matrix object that can cache its inverse
## and then compute the inverse of that matrix object, if not already
## calculated. Otherwise the original matrix is retrieved.

# Written for Rprogramming Coursera course, Assignment 2
# N. Goebel 23 July 2014

## A function to create a special "matrix" object that can cache 
##   its inverse.
makeCacheMatrix <- function(x = matrix()) {

    # ensure that input is correct
    if (!is.matrix(x)){
        stop("be sure to provide a matrix")
    }

    # initialize the inverse object
    im <- NULL

    # a list of functions that:

    # 1. set the value of the matrix x
    set <- function(y = matrix()){
        x <<- y
        im <<- NULL
    }
    # 2. get the value of the matrix x
    get <- function(){
        x
    }
    # 3. set the value of the inverse matrix
    setim <- function(i){
        im <<- i
    }
    # 4. get the value of the inverse matrix
    getim <- function(){
        im
    }
    # Lastly, create a list
    list(set=set, get=get, setim=setim, getim=getim)
}

## A function to compute the inverse of the special "matrix" 
##  returned by makeCacheMatrix.R. 
##  If inverse has already been calculated (and matrix not changed)
##  then the cachesolve should retrieve the inverse from the cache.
## Computing inverse of square matrix can be done with solve().
## Always assumes matrix is invertible.

cacheSolve <- function(x, ...) {
        im <- x$getim()

        # check to see if matrix exists
        if(!is.null(im)){
            message("getting cached data")
            #if exists, return the inverse matrix 
            return(im)
        }
        # if inverse matrix does not exist, calculate and return it
        dat <- x$get()
        im  <- solve(dat)
        x$setim(im)
        im
}
 
 # to test:
# 1. source("cachematrix.R")
# 2. x <-matrix(1:4, 2, 2)
# 3. m <- makeCacheMatrix(x)
# 4. m$get()   # shows original matrix
# 5. m$getim() # should be empty
# 6. cacheSolve(m)  # calculates the inverse of the original matrix
# 7. cacheSolve(m)  # retrives the already calculated and cached inverse matrix
