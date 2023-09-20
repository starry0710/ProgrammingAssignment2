{\rtf1\ansi\ansicpg936\cocoartf2709
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 makeCacheMatrix <- function(x = matrix()) \{\
  i <- NULL\
  set <- function(y) \{\
    x <<- y\
    i <<- NULL\
  \}\
  get <- function() x\
  setinverse <- function(inverse) i <<- inverse\
  getinverse <- function() i\
  list(set = set,\
       get = get,\
       setinverse = setinverse,\
       getinverse = getinverse)\
\}\
cacheSolve <- function(x, ...) \{\
  i <- x$getinverse()\
  if (!is.null(i)) \{\
    message("getting cached data")\
    return(i)\
  \}\
  data <- x$get()\
  i <- solve(data, ...)\
  x$setinverse(i)\
  i\
\}\
> B <- matrix(c(1,2,3,4),2,2)\
B1 <- makeCacheMatrix(B)\
cacheSolve(B1) \
}