# Problem 1
# https://projecteuler.net/problem=1
# If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
# Find the sum of all the multiples of 3 or 5 below 1000.

# Answer: mults(3,5,n=1000)

mults <- function(...,n){
  
  elements <- list(...)
  container <- matrix(0)
  
  for (i in seq_along(elements)) {
    for (j in 1:(n-1)) {
      if (j %% elements[[i]] == 0 & j %in% container == FALSE){
        container <- rbind(container,j)
      }
    }
  }
  return(sum(container))
}