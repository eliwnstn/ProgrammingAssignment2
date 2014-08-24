## makeCacheMatrix creates a list of functions. 
##set "sets" a new matrix and clears the inverse variable i
##get "gets" or returns the matrix.
##setinverse caches the inverse
##getinverse "gets" or returns the inverse

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse)i<<- inverse
        getinverse<-function()i
        list(set=set,get=get,
             setinverse=setinverse,getinverse=getinverse)

}


## cacheSolve checks to see if the inverse was already
##cached. If it was it returns the cached index, if it
##wasnt it calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data)
        x$setinverse(i)
        i
}
