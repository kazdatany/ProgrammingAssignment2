# =============================================================================
# For this assignment, two functions are reuiqred:
#
# Function: makeCacheMatrix
# This takes a matrix as an only argument
# and returns a list of  functions required to cache the invrse of a matrix.
# (note that the matrix supplied assumed to be invertible)
#
# Function: cachSolve
# This takes a returned object of makeCacheMatrix function above
# and calls its functions to see if the inverse of a matrix has been cached.
# If cached, simply returns the cached inverse.
# If not, calculates the inverse, caches it and retrunes the cached inverse.
# =============================================================================

#
# -- Description --
# This function takes a matrix as its only argument, and defines four sets of
# functions to get/set an matrix and its inverse from/to variables.
# It returns the list of these functions.
#
# -- Arguments --
# x: matrix (assume to be invertible)
#
# -- Return Value --
# list of functions
#

makeCacheMatrix <- function(x = matrix()) {
    im<-NULL # this is the variable to cache the inverse of a matrix

    # This function is to set the initial matrix and resets its inverse
    setm<-function(y){
        x<<-y # overwrites a matrix with the newly given argument
        im<<-NULL # and resets the cache to NULL
    }

    # This function is to get the initial matrix
    getm<-function() x

    # This function is to store the given inverse of the matrix
    # into im variable which resides in its parent environment
    setinvm<-function(invm) im<<-invm

    # This function is to get the inverse of the matrix
    # that's stored in the im variable
    # (since im is not defined locally to this function,
    # it goes up to its parent frame and find the value of im)
    getinvm<-function() im

    # Constructs and return a list of 4 functions defined above
    list(setm=setm, getm=getm, setinvm=setinvm, getinvm=getinvm)
}


#
# -- Description --
# This function takes an output from the function makeCacheMatrix(x)
# defined above as its only argument, and
# checks to see if the inverse of a matrix x has already been calculated.
# if so, just returns its cached result; else, computes its inverse,
# saves it into its cache variable and returns it
#
# -- Arguments --
# x: makeCacheMatrix object
#
# -- Value --
# inverse of a given matrix
#
# -- Assumptions --
# . The matrix supplied is always invertible; thus, no error handling exists
# . The function solve(x) can be used to generate inverse matrix
#

cacheSolve <- function(x, ...) {
    # Call "getinvm()" function of x and store its result into im variable
    im<-x$getinvm()

    # If already cached, just return the cached value.
    # Note that !is.null(im) test alone gurantees that the matrix hasn't changed
    # becuase in order to change the matrix, one needs to call x$setm(y)
    # which not only changes the matrix to y, but also, resets im to NULL
    # effectively makes this test fail
    if(!is.null(im)){
        message("getting cached data")
        return(im)
    }

    # else, call "getm()" function of x so that the original matrix can be
    # obtained and stored into data variable
    data<-x$getm()

    # Call solve function to calculate the inverse of the data marix
    im<-solve(data, ...)

    # Call "setinvm(im)" function of x to cache the calculated inverse matrix
    x$setinvm(im)

    # Show the calculated inverse matrix
    im
}

