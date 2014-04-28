## There are two functions used for the program i.e makecacheMatrix and cachesolve
## Function 1 i.e makecacheMatrix will take as argument a matrix nd store the
## inverse of it in an object using scoping rules i.e setting and getting 
## environment variables

## second function will use first function..it returns the matrix inverse by first
## checking it in cache if present immediately returns result nd save time.
## if not it calculates the result nd then return the inverse

## makeCacheMatrix function is taking as argument a matrix nd then using functions 
## inside scope of makeCacheMAtrix getting nd setting the values for Matrix

makeCacheMatrix <- function(x = matrix()) 
{
        CacheMatrix <- NULL 
        set<- function(y){
                x <<- y
                CacheMatrix <<- NULL
        }
        
        get <- function() x
        
        setMatrix <- function(parmMatrix) CacheMatrix <<-parmMatrix
        getMatrix <- function() CacheMatrix
        list(set=set,get=get,setMatrix=setMatrix,getMatrix=getMatrix)
}


## cacheSolve is function which takes as argument matrix it checks whether
## the inverse of matrix is alreadyin cache it returns it in very less time 
## if not present then it sets the matrix compute inverse using solve function
## nd then returns the value of inverse

cacheSolve <- function(x) {
        
        CacheMatrix <- x$getMatrix()
        if(!is.null(CacheMatrix)){
                message("getting cached data")
                return (CacheMatrix)
                
        }
        data <- x$get()
        CacheMatrix <- solve(data)
        
        x$setMatrix(CacheMatrix)
        CacheMatrix        
        ## Return a matrix that is the inverse of 'x'
}
