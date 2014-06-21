## The main objective of this assignment is to write two functions:
## 1. A function that will generate a matrix that can cache its inverse
## 2. A function that will compute the inverse of the matrix passed into (1). If the inverse has already been calculated and the matrix did not change than (2)
##   should retrive the inverse from the cache.

##   The main purpose is to save time on time consuming and expensive computations such as computing the inverse of a large matrix if this has already been done,
##   by caching the inverse computation and calling it when needed if the matrix did not change.


## The function "makeCacheMatrix" does the following:
## 1. Set the values for the matrix
## 2. Get the values of the matrix
## 3. Set the values of the inverse matrix
## 4. Get the values of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #empty matrix for the inverse of the matrix
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
                
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)                
}

## The function "cacheSolve" calculated the inverse of the matrix from the matrix passed into "makeCacheMatrix".
## However this function will first check to see if the inverse has already been computed.
## If so, it gets the inverse from teh cache and skips the computation.
## Otherwise the fucntion will calculate the inverse of the matrix and assign this inverse in the cache via "setmean" function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getmean()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setmean(inv)
        inv
}

## Example of Implementing the Functions

## a<-makeCacheMatrix(replicate(10,rnorm(10)))
## a$get()
##           [,1]       [,2]        [,3]        [,4]       [,5]        [,6]        [,7]        [,8]       [,9]      [,10]
##[1,] -0.366113544 -0.6554023  0.06289353 -0.74614583 -1.3169067 -0.27272126 -0.12966769 -0.63047362 -0.1265939 -0.1837802
##[2,] -0.788975748  0.1903973  0.16671242  0.74782152  0.3102236  0.37617177 -0.64099388  0.48512516  0.3099672  0.6211785
##[3,]  1.714264329  1.1870194 -2.25850107  2.23004286  0.8602623 -0.26841394 -1.17870695  2.30945545 -0.8156295  1.3221813
##[4,] -1.552485851 -0.4894198  0.15415621 -0.11777068 -0.9395096  0.28029586  0.97533398  0.90846354  1.1197458 -0.5171453
##[5,]  2.004407877 -0.6160689 -1.04572407 -0.12098872 -0.4784521 -0.62928020  1.65769533  1.24276879 -0.8582317 -0.9567003
##[6,] -1.381173734 -0.3654388  0.73433429  0.75361105 -0.2012087 -0.10007982  0.06331635  0.40708924 -1.7112764  0.5975126
##[7,]  2.034136312 -1.8442007 -0.56966118 -1.12213397  0.6118146 -0.87604066  1.17654541 -0.08408258 -0.5897542 -0.5415926
##[8,]  0.603372498 -0.5807831  2.17319231  1.28885050  0.2387629  0.14798543  1.17608716  1.10475148  0.9037016  1.4400206
##[9,] -0.899116373 -0.2281555 -0.90707304 -0.75335366 -0.8985923  0.05860263 -0.72134996  0.08124289 -0.6647542  1.5310938
##[10,] -0.004571679  0.5230187  2.03350075  0.01160446  0.1102123  1.23283108 -0.18745798  1.01603348 -0.3148373 -1.6046660

## Before running "to cacheSolve" function
##> a$getmean()
##NULL

## Running the "cacheSolve" function passed into "makeCacheMatrix" function.
##> cacheSolve(a)
##         [,1]        [,2]        [,3]       [,4]        [,5]        [,6]        [,7]        [,8]        [,9]        [,10]
##[1,]  0.19804630  0.85066593 -0.22032733 -0.5313189  0.56970990 -0.31059023 -0.14281312  0.08874869  0.04162061  0.008551988
##[2,] -0.45414172 -2.99684753  0.64863175  0.6386113 -1.06777700  0.18836169 -0.12763863  0.13606750  0.08059979  0.169383576
##[3,]  0.16240629 -2.13407105  0.58808567  0.5329111 -1.14440096  0.27679899  0.29719439  0.28934327 -0.10388377  0.313687207
##[4,]  0.36894654  3.12601026 -0.74479388 -0.9484048  1.51533437 -0.12578001 -0.54453057 -0.11892299 -0.41404647 -0.408460132
##[5,] -0.60867644 -0.79027526  0.21539274  0.3237069 -0.65881182  0.17037521  0.41508653 -0.06047403 -0.03918233  0.061413908
##[6,] -0.35831872  7.05241654 -2.24940967 -2.2699561  3.69616094 -0.92364075 -1.09120457 -0.36015339  0.53819706 -0.339749345
##[7,] -0.62018614  0.94864858 -0.65208640 -0.3642755  1.00021200 -0.05292419 -0.43607988  0.01716479  0.14334837 -0.298308825
##[8,] -0.05224089 -2.76218068  1.02448608  1.1612352 -1.47353951  0.27892620  0.58184064  0.17028166  0.10818832  0.448663640
##[9,]  0.14075281 -0.54468062  0.25895948  0.4098857 -0.48971176 -0.23218909  0.13751707  0.11457698 -0.14031035 -0.017656237
##[10,] -0.24544624 -0.04993526 -0.08743181 -0.1461980  0.05173776 -0.05742406 -0.08598736  0.21238541  0.38159105 -0.107844002


##> cacheSolve(a)
##getting cached data
##         [,1]        [,2]        [,3]       [,4]        [,5]        [,6]        [,7]        [,8]        [,9]        [,10]
##[1,]  0.19804630  0.85066593 -0.22032733 -0.5313189  0.56970990 -0.31059023 -0.14281312  0.08874869  0.04162061  0.008551988
##[2,] -0.45414172 -2.99684753  0.64863175  0.6386113 -1.06777700  0.18836169 -0.12763863  0.13606750  0.08059979  0.169383576
##[3,]  0.16240629 -2.13407105  0.58808567  0.5329111 -1.14440096  0.27679899  0.29719439  0.28934327 -0.10388377  0.313687207
##[4,]  0.36894654  3.12601026 -0.74479388 -0.9484048  1.51533437 -0.12578001 -0.54453057 -0.11892299 -0.41404647 -0.408460132
##[5,] -0.60867644 -0.79027526  0.21539274  0.3237069 -0.65881182  0.17037521  0.41508653 -0.06047403 -0.03918233  0.061413908
##[6,] -0.35831872  7.05241654 -2.24940967 -2.2699561  3.69616094 -0.92364075 -1.09120457 -0.36015339  0.53819706 -0.339749345
##[7,] -0.62018614  0.94864858 -0.65208640 -0.3642755  1.00021200 -0.05292419 -0.43607988  0.01716479  0.14334837 -0.298308825
##[8,] -0.05224089 -2.76218068  1.02448608  1.1612352 -1.47353951  0.27892620  0.58184064  0.17028166  0.10818832  0.448663640
##[9,]  0.14075281 -0.54468062  0.25895948  0.4098857 -0.48971176 -0.23218909  0.13751707  0.11457698 -0.14031035 -0.017656237
##[10,] -0.24544624 -0.04993526 -0.08743181 -0.1461980  0.05173776 -0.05742406 -0.08598736  0.21238541  0.38159105 -0.107844002

## End of the example which shows that when the same matrix is passed into the "cacheSolve" function it retrives the data stored in the cache.