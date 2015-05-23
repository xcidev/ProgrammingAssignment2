## These functions provide an special matrix whose
## inverse value can be consulted and cached.


## Creates the special matrix.
## - Receives a normal invertible matrix.
## - Returns a list of getter and setter functions
##   that operate over the passed matrix
makeCacheMatrix <- function(x = matrix()) {

    #initialize the cached inverse to NULL
    inv <- NULL

    
    #Define initializator of special matrix
    set <- function(y){
        #x holds the original matrix
        x <<- y

        #cached inverse is reset to NULL
        inv <<- NULL
    }

    #Define getter function for the original matrix
    get <- function() x

    #Define inverse cache setter function
    setinv <- function(inv) inv <<- inv

    #Define inverse cache getter function
    getinv <- function() inv 

    #Return the special matrix 
    #which is a list of the functions defined above
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Calculate the inverse of an special matrix created with
## the makeCacheMatrix function.
##  If the inverse was previously calculated, a cacehd value 
##  is returned, else, the inverse is calculated and cached.
cacheSolve <- function(x, ...) {
    #Try getting a cached inverse value
    inv <-x$getinv()


    if(!is.null(inv)){
        #cached value found, return it
        message("getting cached inv")
        return(inv)
    }

    #cached not found, must calculate
    
    #First, get original matrix data
    data <- x$get()

    #then calculate inverse
    inv <- solve(data, ...)

    #save this into cache for later consults
    x$setinv(inv)

    #finally, return calculation
    inv
}


