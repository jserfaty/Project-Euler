# Problem 3
# https://projecteuler.net/problem=3
# The prime factors of 13195 are 5, 7, 13 and 29.
# What is the largest prime factor of the number 600851475143 ?

# Answer: primefactors(600851475143)

primefactors <- function(n){
  factorList <- NULL
  for (i in round(sqrt(n)):2){
    if (n%%i==0){
      factorList <- append(factorList,i)
    }
  }
  
  isprime <- 0
  for (j in 1:(length(factorList)-1)){
    for (k in length(factorList):j){
      if(factorList[[j]]%%factorList[[k]] != 0){
        isprime <- isprime + 1
      }
    }
    if (isprime == length(factorList)-j){
      return(factorList[[j]])
    } else {
      isprime <- 0
    }
  }
}
