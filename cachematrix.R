
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL     #creating a variable called "i" and assigning to it 'NULL' within makeCacheMatrix to be used
  set <-function(y){   #creating a function 'set' with variable 'y'
    x<<-y       #taking a variable from the parent environment
    i<<-NULL   #assigning 'NULL' to 'i' in the parent environment
  }
  get<-function()x #getter and setter function
  setinversed<-function(solve) i<<-solve  #assigning invertion to the setter
  getinversed<-function() i #getter
  list (set= set, get = get, setinversed = setinversed, getinversed = getinversed) #setting names to use '$' sign later
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i<-x$getinversed()  
  if(!is.null(i)) {  #if we've already computed 'i', it will be returned
    message("cached data")
    return(i)
  }
  data<-x$get() #otherwise, we'll compute the new 'i'
  i<-solve(data, ...)
  x$setinversed(i)
  ## Return a matrix that is the inverse of 'x'
  i
}
