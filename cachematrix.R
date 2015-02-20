## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Example
## mTest <- matrix(c(1,2,3,4),2,2 )
##  k = solve(m)
## m_caching <- makeCacheMatrix(mTest)
## m_caching <- cacheSolve(mTest)
## m_caching <- cacheSolve(mTest)

testCaching <- function(x) {
    mTest <- matrix(c(1,2,3,4),2,2 )
    print(mTest)
    m_solve <- solve(mTest)
    print("Using Inverse")
    print(m_solve)
    print("using Caching First Time")
    m_cache<- makeCacheMatrix(mTest)
    m_c1 <- cacheSolve(m_cache)
    print("using Caching Second Time")
    m_c2 <- cacheSolve(m_cache)
    print(m_c2)
    print("Equal if Zeros?")
    mZero<- m_c2- m_solve
    print(mZero )
}
    


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m) 
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
    
}
