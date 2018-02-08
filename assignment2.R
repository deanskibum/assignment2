## Function makeCacheMatrix
## created as assignement 2 for the Proggramming in aR class
## The  the purpose of the lesson was to demonstrate lexical scoping.
##  once the function was instantiated it created an object which Head to the variables
## x and m  within its own environment.
makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, ## creating mutators
             setinverse = setinverse,
             getinverse = getinverse)
}
## function cacheSolve 
cacheSolve <- function(x, ...) {
        m <- x$getinverse() 
        if(!is.null(m)) { ## if the inversion has beed done for this matrix, retrive it
                message("getting cached data")
                return(m)
        }
        data <- x$get() ## otherwise, solve for the new inversion and store it
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
## Creating Test cases
## from Simple test matrices for the lexical scoping programming assignment
## Create testing Matrix
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
## calulate inverse matrix via inverse linear algbra
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
n1
## confirm the two matrixes are inverses of each other
m1 %*% n1
n1 %*% m1
solve(m1)
solve(n1)
## now  instantiate my object and invoke cacheSolve
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
## Demonstrate that if I do it again, the cached data is returned
cacheSolve(myMatrix_object)
