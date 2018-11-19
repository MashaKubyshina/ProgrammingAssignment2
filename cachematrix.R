## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Create a function that sets and gets the value of the cache of the matrix
##and sets and gets the inverse value of the matrix
##It returns a sepcial 'vector' that is a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set <- function(y){
        x<<-y ##Set the value
        i<-NULL ##Clear the cache
    }
    get <-function()x  ##Define function
    setInverse <- function(inverse)  i<<-inverse ##Set the inverse if there is no cache
    getInverse <- function(inverse) i ##Get the inverse
    list (set=set, get=get, setInverse=setInverse, getInverse=getInverse) ##Return the list of values

}


## Write a short comment describing this function

##This function calculates the inverse of a 'vector' from the previous function
##It first checks if inverse has already been calculated 
##If so it takes the inverse from the cache and skips computation
##If not, it calculates the inverse of the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    c<-x$getcache() #looks for the cache
    if(!in.null(c)) {
        message("getting cached data") #checks if cache exists, if it does, the get cache data
        return(c) #return cache data
    }
    ## If the cache empty, then calcualte it
    data <-x$get ## get the value
    c<-solve(data, ...) ##get the inverse of the data
    x$setinverse(c) ##set cache for x
    c ##print value of c
    
}
