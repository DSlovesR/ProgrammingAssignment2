## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix() function creates matrix and stores matrix and its iverse 
##have function to get/set matrix and inverse

##cacheSolve() function return the inverse of matrix of the
##object created using makeCacheMatrix() 


## Write a short comment describing this function

## makeCacheMatrix() has two object to hold matrix and its inverse 
## and following function:
## set() - set the value of matrix in global environment using <<- 
## get() - get the value of the matrix
## setInverse() - set inverse of the matrix in environment 
## getInverse() - retrive cached inverse matrix if any

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    ## set the input matrix in global env
    set  <- function(y) { 
          x <<- y  
       invM <<- NULL 
    }
    
    ## return global 
    get  <- function() x
    
    setInverse <- function( mInverse) { invM <<- mInverse}
    getInverse <- function() invM
    
    list( 
       set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse
       )
}


## Write a short comment describing this function

## cacheSolve() function returns the inverse of matrix from 
## cache stored in global environment by makeCacheMatrix()
## This function first retrives already computed inverse matrix
## from global. In case getInverse() returns null then cacheSolve() 
## calculate inverse usign solve() function and set to global enviroment
## by calling setInverse() function prior to return result.

cacheSolve <- function(x, ...) {
   cacheVal <- x$getInverse()
   if ( !is.null( cacheVal )) {
      message("getting cached inverse of matrix")
      return( cacheVal)
   }
   
   cacheData <- x$get()
   cacheVal <- solve(cacheData)
   x$setInverse( cacheVal)
   ## Return a matrix that is the inverse of 'x'
   cacheVal
}


# ## test steps and outputs for references purpose only
# # 
#  mat <-  matrix(c(4,5,6,7), nrow = 2, ncol = 2)
#  xx<- makeCacheMatrix()
#  xx$set(mat)
# 
#  xx$get()
##[,1] [,2]
##[1,]    4    6
##[2,]    5    7
# 
#  cacheSolve(xx)
##[,1] [,2]
##[1,] -3.5    3
##[2,]  2.5   -2
