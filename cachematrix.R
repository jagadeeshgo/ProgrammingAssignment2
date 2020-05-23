## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creating a special type of cacheble matrix
makeCacheMatrix <-function(x=matrix()){
  inverse <- NULL                   #inverse will store in this matrix,primarily it is null
  set <-function(y){                #this function set the value of a vector
    x <<- y                         #update to the new matrix
    inverse <<- NULL                #resets inverse of the matrix of the new matrix
  }
  get <- function()x                #this function gets the matrix
  setinverse <- function(solve)inverse  #set the value of the matrix
  getinverse <- function()inverse       #get the value of the matrix
  list(set=set, get=get,                #list of available function
       setinverse=setinverse,
       getinverse=getinverse)
}

## Write a short comment describing this function
#calculates the inverse of the matrix of the above the which we created
cacheSolve <- function(x, ...) {
  inverse  <- x$getinverse()            #holds the inverse matrix
  if(!is.null(inverse)){                #checks if it is claculated or not
    message("getting data from the cache")      #gives message if calculated
    return(inverse)                     #returns inverse of the matrix
  }
  data    <- x$get()                    #if the inverse not calculated it will get the matrix
  inverse <- solve(data, ...)           #calculates the inverse
  x$setinverse(inverse)                 #it will update the inverse
  inverse
}
