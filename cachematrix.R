#The following two functions calculate the inverse of a square matrix. 
#If the inverse is already calculated, it gets the cache value and 
#skips the computation.

#The fist function creates a special matrix.
makeCacheMatrix<-function(x=matrix()){
  m<-NULL
  set<-function(y){  #set the value of the matrix
    x<<-y
    m<-NULL
  }
  get<-function() x #get the value of the matrix
  setinverse<-function(inverse) m<<-inverse  # set the inverse matrix
  getinverse<-function() m  #get the inverse matrix
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

#The second function calculates the inverse of the matrix created with the first function.
cacheSolve<-function(x,...){
  m<-x$getinverse()
  if (!is.null(m)) {  #the inverse is already calculated. It skips the caculation.
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
  
}
