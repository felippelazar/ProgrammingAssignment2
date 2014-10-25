makeCacheMatrix <- function(x=matrix()){ ## Creates a function 							in which the argument should be a matrix
	m <- NULL ## It sets the m value to NULL 
	set <- function(y){ 
		x <<- y ## x is set to be y only in parent environments
		m <<- NULL ## In the "set" function, vector "x" is set to 					be vector "y" and the mean is set again to be 					NULL - Important in case you want to change 					the matrix you've put as "x" (making m NULL 					again will help you not get the wrong inverse 					matrix)
	}
	get <- function() x ## Return the vector x
	setinverse <- function(solve) m <<- solve #sets the inverse, 												"solve", to m
	getinverse <- function() m ## Returns the m that is the 									inverse (result of function 									"solve")
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## Returns the "special vector" with all 						the functions set above
}

cacheSolve <- function(x, ...){ ## This function will give the 								"inverse" of the matrix if not 									already "solved" (using the 									function "solve")
	m <- x$getinverse() ## It checks if the m was already defined 						(if the inverse was already obtained)
	if(!is.null(m)){ 
		message("getting cached data")
		return(m) ## Creates a condition that if m wasn't NULL (or 					it was already obtained), it should get it 						from the memory (return m)
	}
	data <- x$get() ## Bind the matrix we created in the last 						function to a object called "data"
	m <- solve(data,...) ## Gets the inverse of the matrix "data"
	x$setinverse(m) ## Set the result of inverse to the variable m 					so we can retrieve this result easily from the 					other function (cached result)
	m
}

## Basically these two functions work together and in sequence to determine the inverse of the matrix. The first one - makeCacheMatrix stores the inverse which is a result of the second function - cacheSolve. So basically when you call the latter function, it looks at the "special vector" to see if it has the result, in our case, m. If not, it calculates and stores ir at the special vector. Thus, everytime you ask for it, the second function will try to retrieve from the first one or calculate it and store it there. 
