#1 Write a function to evaluaele a^b given a and b. The function should have a default value of 3 for b.
myfunc1 <- function(a, b=3) {
  a^b
}
myfunc1(2)

#2 Write a function that accepts two arguments, a number and a vector, and returns TRUE if the number is inside the vector.

myfunc2 <- function(a,b) {
  for (i in b) {
      if (i == a) {
        return(TRUE)
      }
    }
}

myfunc2(7,c(1,2,3,7))
  
#3 Write a function that, given a number and a vector, will return the number of times the number occurs in the vector.


myfunc3 <- function(a,b) {
  x <- 0 
  for (i in b) {
    if (i == a) {
      x <- x + 1 
    }
  }
   return(x)
  }

myfunc3(3, c(1,3,3,3,3,5))

#4 Use the mtcars dataset for this question. Use one of the apply functions to find the mean of every column in mtcars.
sapply(mtcars, FUN=mean)

#5 Write a function in R that takes as input a non-negative integer n and returns Fn. Use a for
# loop in the function. Then use the function to find F20 and F80. 1

myfunc4 <- function(x) {
  oldnum <- 0 
  newnum <- 1
  if (x==0) {
    return(oldnum)
  }
  else if (x==1) {
    return(newnum)
  } else {
    for (i in 2:x) {
      newestnum <- newnum + oldnum 
      oldnum <-  newnum
      newnum <- newestnum
    }
    return(newestnum)
  }
}

myfunc4(20)
myfunc4(100)

#6 Write an R function to compute fn(x) for any real number x and positive integer n. Use a while loop to do this. The function should return a vector with two numbers, the value of fn(x) and the approximation error ex − fn(x). Run your function for x = 3, n = 10 and x = 0.44, n = 3.

myfunc5 <- function(x,n) {
  cake=0
  r=0
  while(r<=n) {
    cake= cake + (x^r)/factorial(r)
    r<-r+1
  }
  return(c(cake,exp(x)-cake))
}
myfunc5(3,10)
myfunc5(0.44,3)

#7 Write two R functions to compute the factorial of any user-supplied positive integer n. The first function should calculate (and return) n! using a for loop. The second function calculates and returns n! recursively.

myfunc6 <- function(n) {
  gum=1
  for (i in seq_len(n)) {
    gum = gum * i
  }
  gum
}

myfunc6(3)  

recursive7 <- function(p) {
  if (p==1 || p==0) {
  return(1) }else{
    recursive7(p-1)*p
  }
}

recursive7(5)
#8 Write a function that accepts a vector of numbers (of length >= 4) and returns a vector of moving averages, i.e. with x = (x1, . . . , xn), the output is a vector with the values

myfunc8 <- function(y) {
  if (length(y) >= 4) {
    t <- 0
    finalvector <- c() 
    for (i in 1:length(y)-3) {
      t <- (y[i] + y[i+1]+ y[i+2] +y[i+3])/4
      finalvector <- c(finalvector,t)
    }
  }
  finalvector
  }
                                          
myfunc8(y=c(1, 1, 2, 5, 8, 3, 4, -4, 3, 7, 2, 2, -2, 1))

#9 Write an R function that accepts a vector of numbers and returns the values of the function f (x)

myfunc9 <- function(e) {
  vector1 <- c()
  for (i in e) {
    if (i<0)  {
      ans = i^2 + 2*i + 3
      vector1 <- c(vector1, ans) 
    } else if (0<=i || i<2) {
      ans = i + 3 
      vector1 <- c(vector1, ans)
    } else {
      ans =  i^2 + 4*i - 7 
      vector1 <- c(vector1, ans)
    }
  }
     plot(vector1)
  return(vector1)
}  

myfunc9(c(-2,-1,0,1,2,3))

#Generate a dataset using the code below and then follow the example in Lecture set 8 to create an objective function based on the generated dataset and obtain estimates for a and b. Select your own starting values.

set.seed(20330);
mydata <- rgamma(100, shape=4, scale=5)  


make.NegLogLik <- function(data, fixed=c(FALSE, FALSE)){
  params <- fixed
  function(param.values){
    params[!fixed] <- param.values
    mu <- params[1]
    sigma <- params[2]
    e <-(mu-1) * sum(log(data)) - length(data) * log(gamma(mu)) - (length(data) * mu) * log(sigma) - (1/sigma) * sum(data)
    e
  } }
my.negloglik <- make.NegLogLik(mydata)
  
optim(c(mu=4,sigma=5), my.negloglik)$par
  
  





