######### Create a simulation of a single elimination tournament with 64 teams ##########
######### I plan on using this small outline in upcoming March Madness (with some edits to make it
######### like the actual tournament instead of this fake tournament style). ############

set.seed(424)
ratings <- rev(sort(rgamma(64, 3, 3)))
n <- 100000
winners <- vector()

tourney <- function(ratings){
  myratings <- ratings
  for(j in 1:6){
    for (i in 1:(length(myratings)/2)){
      probWin <- 1/(1+exp(-(myratings[i]-myratings[length(myratings)-i+1])))
      ifelse(rbinom(1,1,probWin) == 0 , myratings[i] <-  0, myratings[length(myratings)-i+1] <- 0)
    }
    myratings <- myratings[myratings!=0] # get rid of teams that lost
  }
  return(match(myratings,ratings)) # return the team number (1 through 64)
}

for (w in 1:n){                    # simulate 100000 times
 winners[w] <- tourney(ratings)
}

myTable <- data.frame(table(winners))
myTable$Freq <- myTable$Freq/n   # This is all to make pretty output
    #return the probabilities for all the winners
Problem1 <- data.frame(ratings,myTable$Freq)
names(Problem1)<-c("Ratings","Probability")
Problem1