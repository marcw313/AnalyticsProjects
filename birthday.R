#The Birthday Problem concerns the probability, in among n randomly chosen people, some pair 
#of them will have the same birthday. Write an R function called birthday that takes two arguments:
#n (a numeric vector of length one giving the number of people) and nReps giving the number of 
#replications for the simulation study. The function should return a numeric vector of length 
#three giving a Monte Carlo estimate and a 95% confidence interval on the probability. 
#Name your script 'birthday.R'.

birthday <- function(n,nReps){
              #simulate nReps of how many birthdays are the same
  number.matching <- sapply(1:nReps, 
        function(x) {n-length(unique((sample(1:365,n,replace = TRUE))))}) 
  x <- 0  ## create a vector x for storage later.
  for (i in 1:nReps){
    if(number.matching[i] == 0) { x[i] <- 0 }
    else { x[i] <- 1 }      ## 1 means success of at least 1 birthday couple. Use to simulate probability
  }
  p.hat <- mean(x)          ## simulation probability of at least 1 bday couple among n people.
  ci <- p.hat + c(-1,1)*qnorm(1-(0.05/2))*sqrt((p.hat*(1-p.hat))/nReps) ##confidence interval for probability
  out <- c( p.hat , ci )
  names(out) <- c("Estimated Probability", "CI Lower Bound" , "CI Upper Bound")
  out
}

