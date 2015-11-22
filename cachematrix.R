## this function retunrns an object with getter and setter function 
## the object has two matrices as properties
makeCacheMatrix <- function(x = matrix()) {
	invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseofx) invx <<- inverseofx
        getinverse <- function() invx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## function accept an object returned by makeCacheMatrix.
## It checks if property invx of arguments object is inverse matrix of property x. If it is, it returns invex (cached inversed matrix). 
## Otherwise it finds invers matrix of property x and set it as property invx and returns it. 
## function assumes that propery x of arguments is a square matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	data <- x$get()	
	invx <- x$getinverse()
        if(!is.null(invx)) {
		##checking if data is not chnaged after invered cached
		##first check - checking if both matrix are square matrix of same size
		if(nrow(invx) == ncol(invx) & nrow(invx) = nrow(data)){
			##second check
			temp <- invx[,1] %*% data[1,]
			if(temp[1,1] == 1){
                		message("getting cached data")
                		return(invx)
			}else{
				##we can remove this else block if we do not want to see why cached matrix is not returned. 
				message("Matrix is changed, so getting new inverse")
			}
		}
        }
        
        invx <- solve(data)
        x$setinverse(invx)
        invx
}
