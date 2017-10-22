## The following two functions are my solution to Coursera's Week3 Peer-graded Assignment: Programming Assignment 2: Lexical Scoping

## This function creates a special "matrix" object that can cache its inverse.
## NOTE: This is similar to Java's getters and setters.

makeCacheMatrix <- function(x = matrix()) { #initialize object: matrix "x"
        inv <- NULL #initialize object: NULL object"inv", reserved for the cacheSolve function.
        set <- function(y) { #
                x <<- y
                inv <<- NULL
        } #NOTE: This whole "set" part can be omitted, if I don't need to set the matrix to another matrix.
        get <- function() x #GET the inputted matrix x
        setinverse <- function(inverse) inv <<- inverse #IMPORTANT: SETS (receives) the "inv" calculated in the cacheSolve function.
        getinverse <- function() inv #GET the "inv" value that has been loaded above.
        list(set = set, #Name the set function as "set", and so on...
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { #x will be the name of the matrix function I've created using makeCacheMatrix
        inv1 <- x$getinverse() # initialize object "inv1". 
         #I've put in 1 because the name of inv doesn't have to be same with inv in makeCacheMatrix. (it can also be the same.)
        if(!is.null(inv1)) {
                message("getting cached data")
                return(inv1)
        }
        data <- x$get() #initialize object "data" and load the input matrix from makeCacheMatrix
        inv1 <- solve(data, ...) #initialize object "inv1" and calculate the inverse matrix.
        x$setinverse(inv1) #call the setinverse() function in makeCacheMatrix function, 
        inv1 #print the solution
}



##SOLUTION CHECK###

myMatrix <- makeCacheMatrix(matrix(c(1,2,3,4),nrow = 2,ncol = 2)) #Make myMatrix
myMatrix$get() #Check if input is alright
#       [,1] [,2]
#[1,]    1    3
#[2,]    2    4
myMatrix$getinverse()
#NULL

cacheSolve(myMatrix) #Get the inverse matrix of myMatrix.
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
myMatrix$getinverse() #Check if the inv matrix has passed onto myMatrix
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

myMatrix$set(matrix(c(10,5,2,6),nrow = 2,ncol = 2)) #Change the matrix value
myMatrix$get() #Check if the change has gone through
#       [,1] [,2]
#[1,]   10    2
#[2,]    5    6

cacheSolve(myMatrix) #Get the inverse matrix of myMatrix.
#       [,1]  [,2]
#[1,]  0.12 -0.04
#[2,] -0.10  0.20
#this is the inv matrix for matrix(c(10,5,2,6),nrow = 2,ncol = 2) (the modified one.)
myMatrix$getinverse() #Check if the inv matrix has passed onto myMatrix
#       [,1]  [,2]
#[1,]  0.12 -0.04
#[2,] -0.10  0.20

#End of solution check#

