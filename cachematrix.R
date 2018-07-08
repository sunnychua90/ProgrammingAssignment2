## This function is meant to cache existing inverse matrix and to call back the cache

makeCacheMatrix <- function(x = matrix()) {
       
         i <- NULL ## Set inverse = NULL
        
                set<-function(y) ##2 lines of code in set() do exactly the same thing as the first two lines in the main function: set the value of x, and NULL the value of i
                {
                y <<- x ## Assign value of x to y in the parent environment
                i <<- NULL ## This line of code clears any value of m that had been cached by a prior execution of cacheSolve().
                }
        
        get <- function() x ## since x not defined within get, it will retrieve from parent environment (i.e. parent matrix)
        
        setinv <- function(inv) {
                i <<- inv  ## Assign the inverse matrix to i in the parent environment
        }
        
        getinv <- function() {
                i
        }
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Pull the cache if inverse was previously catched in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of matrix 'x'
        i <- x$getinv()
        
        ## check if i is not null, get inverse matrix
        if(!is.null(i)) {
        message("Getting cached data")
        return(i)
        }
        
        data <- x$get() ## if no cached data, get existing matrix
        i <- solve(data) ## set i to be inverse of x - solve the inverse using solve function
        
        x$setinv(i) ## Assign the inverse to variable i 
        
        i ## print inverse of x
}
