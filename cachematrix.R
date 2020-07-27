## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invrs<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
get<- function() x
setInverse<-function(solvematrix) invrs<<-solvematrix
getInverse<-function() invrs
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getInverse()
        if(!is.null(invrs)){
                message("getting cached data")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data)
        x$setInverse(invrs)
        invrs   
}
