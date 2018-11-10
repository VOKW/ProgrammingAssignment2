## These 2 functions makeCacheMatrix()and cacheSolve() will cache the inverse
## of a square invertible matrix.  If the matrix inverse is needed again
## it will be retrieved from memory instead of recalculated.  If the matrix
## is new then the inverse will be calculated and stored.

## makeCacheMatrix() takes the argument "mat" which names the 
## matrix.  This function creates a list of functions get(), set(),
## getinv(), and setinv().  get() retrives the matrix "mat".  set()
## sets the matrix "mat". getInv() retrieves the cached inverse otherwise
## returns NULL.  setInv() will set "inv" returned from the cacheSolve()
# function below.

makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL
        #Set the matrix
        set <- function(matrix){
                mat <<- matrix
                inv <<-NULL
        }
        #Get the matrix
        get <- function() {
                mat
        }
        #Set the inverse
        setInv <- function(calcinv) {
                inv <<- calcinv
        }
        #Get the inverse
        getInv <- function() {
                inv
        }
        #List of functions
        list(set=set, get=get, setInv=setInv, getInv=getInv)
        
}

## cacheSolve() checks to see if there is an inverse cached, if not
## it uses the solve() function to calculate the inverse and stores it in 
## "inv"

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of mat
        inv <- mat$getInv()
        if(!is.null(mat)) {
                message("Getting cached data.")
                mat
        }
        data <-mat$get()
        inv <- solve(data,...)
        mat$setInv(inv)
        inv
        
}


