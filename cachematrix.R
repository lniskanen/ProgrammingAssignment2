## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #initialize as NULL
  inverseM<-NULL
  
  #set new matrix
  set<-function(newMatrix){
    x<<-newMatrix
    inverseM<<-NULL
  }
  
  #get matrix
  get<-function() x
  
  #get inverse of matrix x
  getInverse<-function() inverseM
  
  #set calcualted invers of matrix x
  setInverse<-function(z) inverseM<<-z
  
  #return list of functions for special matrix
  list(get=get,set=set,getInverse=getInverse,setInverse=setInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
  cache<-x$getInverse()
  
  #if inverse matrix is calcualted, return it
  if(!is.null(cache)) {
    #    message("return data from cache")
    return(cache)
  }
  
  #if matrix is not cahced, retrieve it, make inverse and set it
  matrix<-x$get()
  x$setInverse(solve(matrix))
  
  ## Return a matrix that is the inverse of 'x'
  solve(matrix)
}




#Let's create some random matrix
m1<-matrix(data = rexp(200, rate = 10), nrow = 10, ncol = 10)

#Turn it to special matrix
sm<-makeCacheMatrix(m1)

#get inverse first time
cacheSolve(sm)

#get inverse from cache on second round
cacheSolve(sm)

#Check to see that Identity matrix is really returned
round(sm$get()%*%cacheSolve(sm))

#Compere caching speed with different matrix size

# I=500
# 
# meas=matrix(NA,ncol=6,nrow=I)
# 
# for (i in 2:I){
#   m<-makeCacheMatrix(matrix(data = rexp(i*i, rate = i), nrow = i, ncol = i))
#   
#   t0<-proc.time()
#   cacheSolve(m)
#   t1<-proc.time()
#   cacheSolve(m)
#   t2<-proc.time()
#   
#   meas[i,1:3]<-(t1-t0)[1:3]
#   meas[i,4:6]<-(t2-t1)[1:3]
# }

#Print results
# print("Time to inverse matrix first time")
# range(meas[,1],na.rm=TRUE)
# print("Time to get inverse matrix from cache")
# range(meas[,4],na.rm=TRUE)
