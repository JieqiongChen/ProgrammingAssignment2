## Functions to cache inverse of a matrix. 
#Each time it is asked to invert a new matrix it check 
#to see if it has already computed the inverse to avoid
#repetitive recalculation. If the given matrix's inverse 
#has not yet been computed we calculate it and cache the result. 


##This function creates a matrix object 

makeCacheMatrix <- function(x = matrix()) {
	
         #Initialization
	inv <- NULL

	#set the matrx
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	#get the matrix
	get <- function() {
            x
        }

	#Inverse of the matrix
	setInv <- function(inverse) {
               inv <<- inverse
        }

	#Inverse of the matrix
	getInv <- function() {
               inv
        }

	#Return a list of the methods
	list(set = set, get = get, setInv = setInv, getInv = getInv)
	

}


##This function computes the inverse of the matrix
# returned by "makeCacheMatrix". If the inverse has 
#already been calculated (and the matrix has not changed)
# then the "cachesolve" should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

	#Get the inverse of matrix x
	inv <- x$getInv()

	#if it is already set, return the inverse
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}

	#get the matrix from our object	
	data <- x$get()
	
	#calculate the inverse using matrix multiplication
	inv <- solve(data,...)

	#Store the calcualted inverse in the cache for later use 
	x$setInv(inv)

	inv
}
