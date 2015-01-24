## Put comments here that give an overall description of what your
## functions do
## Assignment2 Hongyan Chen 2015
## Note it does not check the invertibility of matrix

## Write a short comment describing this function
## Mimic the sample mean function such that provide a list of functions
## To return the stored matrix 
## To set or store the matrix
## To return the stored inverse matrix
## To set or store the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	inv.m <- NULL
	set <- function (y){
		x <<- y
		inv.m <<- NULL
	}
	get <- function() x
	set.inv <- function(inverse) inv.m <<- inverse
	get.inv <- function() inv.m
	list(set=set,get=get,set.inv=set.inv,get.inv=get.inv)
}


## Write a short comment describing this function
## Check if the inverse matrix is already stored
## If yes directly return it
## Otherwise get the stored matrix and calculate the inverse
## Also store the calculated inverse in the list
## Note that the input is the object defined by the function above not a conventional matrix
## Otherwise it will be an error does not check the input though

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv.m <- x$get.inv()
	if(!is.null(inv.m)) {
		message("getting cached inverse")
		return(inv.m)
	}
	data <- x$get()
	inv.m <- solve(data)
	x$set.inv(inv.m)
	inv.m
}

## Test runs if you like
## mask the following if you dont like
## Initilization
set.seed(100)
x<-cov(matrix(rnorm(100),ncol=4))
x.special<-makeCacheMatrix(x)


## NOTE the makeCachedMatrix does NOT calculate the inverse
x.special$get.inv()
## inverse is calculated and stored by cacheSolve
cacheSolve(x.special)
x.special$get.inv()


## inverse is not needed this time
cacheSolve(x.special)


## Once x is reset and then calculation occurs again so does update 
x.rev<-cov(matrix(rnorm(100),ncol=4))
x.special$set(x.rev)
cacheSolve(x.special)
x.special$get.inv()



