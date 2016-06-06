## This function makeCacheMatrix takes a matrix argument and initialises memory to Null
##sets values of the matrix into mat & initializes memory variable to NULL
##Stores matrix into getvalue, initializes variable memory with inverse&getinverse
##Function cacheSolve checks if the memory variable already has inverse calculated or NULL and based on the TRUE or FALSE traverses into IF clause 
##In case the memory is NULL the cacheSolve function gets inverse calculated and stores into memory to cache it latter
makeCacheMatrix <- function(x = matrix()) {##Argument mat is being passed in calling function
	memory<-NULL		#sets memory variable to NULL
	setvalue<-function(z){  
		x<<-z		# This function setvalue is initializing mat with argument z & setting memory to NULL
		memory<<-NULL
	}
	getvalue<-function()x		## matrix values being stored into variable getvalue
 	setinverse<-function(inverse)memory<<-inverse ## variable memory being initialized with a argument value inverse
	getinverse<-function()memory				##getinverse variable is being set to memory 		
	list(setvalue=setvalue,getvalue=getvalue,setinverse=setinverse,getinverse=getinverse) ##a list of functions being returned 
}




cacheSolve <- function(x, ...) { ## This function takes one data argument 
        ## Return a matrix that is the inverse of 'x'
	memory<-x$getinverse() 	##the function getinverse is being called & is being initialized with the matrix data 
	if(!is.null(memory)){			##If in case the memory variable initialy doesnot have the inverse data then it will not enter the if becase if clause will return False, since is.null will be true & become not true which is false
		message("getting cache values")
		return(memory)
	}
	data<-x$getvalue() ## calls get value and initializes data with the matrix data argument passed in first function call makeCacheMatrix
	memory<-solve(data,...) ## Solve function will provide inverse of matrix and store in memory
	x$setinverse(memory) ## once inverse is calclated it sets the value in memory & memory variable is returned
	memory
}
