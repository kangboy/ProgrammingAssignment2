## A pair of functions to compute and cache the inverse of a matrix.

## Cache the inverse by returning a special "matrix" which is actually a list
## containing four functions.

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set<-function(y){
                x<<-y
                inverse<<-NULL
        }
        get<-function()x
        setinverse<-function(inv)inverse<<-inv
        getinverse<-function()inverse
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Calculates the inverse of a matrix. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the calculation. If not, it computes the inverse 
## and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("getting cached data!")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinverse(inv)
        inv
}
