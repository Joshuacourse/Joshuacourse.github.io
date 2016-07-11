## Question #1:
# Each new term in the Fibonacci sequence is generated by adding the previous two terms.
# Starting with 1 and 2, the first 10 Fibonacci numbers are:
# 1, 2, 3, 5, 8, 13, 21, 34, 55, 89,...
# Consider all the Fibonacci numbers whose values do not exceed four million. Find the sum of even-valued terms among them.
i = 2
x = c(1, 2)
while (x[i] < 4*10^6) {
        x[i+1] = x[i-1] + x[i]
        i = i + 1
}
x[-i]

## Question #2
# Write your own codes to implement the multiplication between a matrix and a vector. In
# case you forget it, here's the definition of matrix multiplication. For an m×n matrix A and
# an n-dimensional vector x, y = Ax is an m-dimensional vector, where
 multiplication=function(x,y){
        stopifnot(is.matrix(x),is.vector(y),mode(x)=="numeric",ncol(x)==length(y))
        n=nrow(x); m=ncol(x)
        for(i in 1:n){
                n[i]=sum(x[i,]*y)
        }
        return(n)
}


## Question #3
# Write your function that calculates the median absolute deviation (MAD) of a numeric
# vector. The median absolute deviation is a robust alternative to standard deviation as a
# measure of dispersion. It is defined as a vector X as
MAD=function(x){
        stopifnot(is.numeric(x))
        n=length(x)
        for(i in 1:n){
                n[i]=abs(x[i]-median(x))
        }
        return(median(n))
}


## Question #4
# Suppose we have a character vector as follows: Names <- "John Andrew Thomas”
# Write some R code to obtain the following output:
# "John@gmail.com ; Andrew@gmail.com ; Thomas@gmail.com"
Vector1= "John Andrew Thomas"
Vector1Split=unlist(strsplit(Vector1,split = " "))
VectorConcatenate=paste(Vector1Split,"@gmail.com",sep = "",collapse = ";")
VectorConcatenate




## Question #5
# Write some R code to generate a vector with the following elements, without using loops .
# "aa" "ba" "ca" "da" "ea" "ab" "bb" "cb" "db" "eb" "ac" "bc" "cc" "dc" "ec"
# "ad" "bd" "cd" "dd" "ed" "ae" "be" "ce" "de" "ee"
Elements=c("a","b","c","d","e")
CreateString1=rep(Elements,5)
CreateString2=rep(Elements,c(5,5,5,5,5))
CreateVector=paste(CreateString1,CreateString2,sep ="")
CreateVector