############################ Problem Instructions ###################################################
# Consider a game in which a player has 5 six-sided dice (where each side is
# equally likely).  The goal is to get the most dice of the same side (but the
# side itself does not matter) after three rounds of rolling.  In the first
# round, the player rolls all 5 dice.  In subsequent rounds, the player may
# roll some or all of the dice.  At the end of the three rounds, the points the
# player gets are the number of dice of the most frequently occuring side.  For
# example, if the player ends the third round with dice "4,4,6,6,4", the player
# receives three points because the most frequently occuring side (i.e., "4")
# occurs three times.

# Clearly describe a strategy for playing the game
# designed to maximize points.  Under your strategy, estimate: 1. the mean
# number of points and 2. the probability of four or more points.


####################################################################################################
################################### Description of game method by Marc Wells #######################
####################################################################################################
# The best method for this is to throw the 5 dice the first round. From the throw, you can get 
# anywhere from 0 matching to 5 matching.
# Find the maximum value of matching dice, and choose that to be your number to gather. From there
# roll the remaining dice another time. Pull out any from your number of choice. Roll the remaining
# dice one last time. Count the number that match your numbers. Because the dice are independent,
# switching your number in between rolls is not adviseable.
# if zero are matching, arbitrarily choose one number that you rolled, and use that as your number


######### creating a function to simulate 3 throws of dice as described in description above
simulate <- function() {
  x <- sample(c(1:6),size=5,replace=TRUE)
  i=1
  xx <- as.data.frame(table(x),stringsAsFactors = F)
  choices <- xx[max(xx$Freq)==xx$Freq,]                   ##finding the largest frequency
  my.choice <- choices[1,1]                               ##in case the vector is greater than 1
  my.choice <- type.convert(sapply(my.choice,as.vector))  ##converting the character value to an integer
  while (i != 3){
    if (length(unique(x)) == 1) { return(5) }             ##if all 5 matching on first or second throws, return score of 5
    else {
      x[x != my.choice] <- sample(c(1:6) , size= length(x[x != my.choice]) , replace=TRUE) ##replace non-matching values with new rolls.
      i = i+1    ##increase iterator for while statement
    } 
  }
score = length(x[x == my.choice])   ##find how many of the 5 match my number of choice
return(score)                       ##return score if not returned previously
}    
    
######### start simulation ##########

num <- 10000
points <- sapply(1:num,function(z) simulate())

## estimate mean number of points
m <- mean(points)
m   #the mean number of points
sd <- sd(points) #for monte carlo error
alpha <- .05
m + c(-1,1)*qnorm(1-(alpha/2))*(sd/sqrt(num))  ##finding monte carlo error


## estimate probability of 4 or more points
p.hat <- mean(points >= 4)
p.hat  ##proportion with 4 or more points. This is an estimate of the probability of 4 or more points
p.hat + c(-1,1)*qnorm(1-(alpha/2))*sqrt((p.hat*(1-p.hat))/num)  ##finding monte carlo error


