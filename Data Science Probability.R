
library(tidyverse) #load also ggplot2
library(dslabs)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(titanic)
library(gtools)

library(mosaic) # Diesen Befehl bei jeder Session am Anfang ausführen
library(readr)
library(gplots)


set.seed(1, sample.kind="Rounding") 

beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 1)    # sample 1 bead at random

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions

##Generating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

##Permutations and combinations
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

##Probability of drawing a second king given that one king is drawn
hands <- permutations(52,2, v = deck)
first_card <- hands[,1] #column 1 of array hands
second_card <- hands[,2] #column 2
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)


##Probability of a natural 21 in blackjack
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))


##Monte Carlo simulation of natural 21 in blackjack

# code for one hand of blackjack
hand <- sample(deck, 2)
hand

# code for B=10,000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

##The birthday problem
# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays

##Function for birthday problem Monte Carlo simulations

# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)
prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)

##Code: Computing birthday problem probabilities with sapply
# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob


##Estimating a practical value of B
B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates

##Code: Monte Carlo simulation of stick strategy
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking

##Code: Monte Carlo simulation of switch strategy
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching

##monte carlo
set.seed(1, sample.kind="Rounding") 
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
m<-replicate(10000,{
  sum(sample(runners,3) == "Jamaica") == 3
})
mean(m)

##meals
comb_meals <- function(e){    
    e * 3 * 6 *5 / 2
}
sapply(seq(1,12),comb_meals)

comb_meals <- function(s){    
  6 * 3 * s * (s-1) / 2
}
sapply(seq(2,12),comb_meals)

## cancer
head(esoph)
nrow(esoph)
all_cases <- sum(esoph$ncases)
all_cases
all_controls <- sum(esoph$ncontrols)
all_controls
boxplot(ncases~alcgp, data = esoph)
boxplot(ncontrols~alcgp, data = esoph)

esoph %>% filter(alcgp == "120+") %>%
   summarize(prob = sum(ncases) / sum(ncases+ncontrols) ) %>%
   .$prob
esoph %>% filter(alcgp == "0-39g/day") %>%
  summarize(prob = sum(ncases) / sum(ncases+ncontrols) ) %>%
  .$prob

boxplot(ncases~tobgp, data = esoph)
cases <- esoph %>% filter(tobgp == "0-9g/day") %>%
  summarize(prob = sum(ncases) ) %>%
  .$prob 
1 - cases/all_cases
 
boxplot(ncases~tobgp, data = esoph)
controls <- esoph %>% filter(tobgp == "0-9g/day") %>%
  summarize(prob = sum(ncontrols) ) %>%
  .$prob 
1 - controls/all_controls


cases <- esoph %>% filter(alcgp == "120+") %>%
  summarize(cases = sum(ncases) ) %>%
  .$cases
a <-cases / (all_cases)

cases <- esoph %>% filter(tobgp == "30+") %>%
  summarize(cases = sum(ncases) ) %>%
  .$cases 
cases/(all_cases)

cases <- esoph %>% filter(tobgp == "30+" & alcgp == "120+") %>%
  summarize(cases = sum(ncases) ) %>%
  .$cases 
cases/(all_cases)

cases <- esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>%
  summarize(cases = sum(ncases) ) %>%
  .$cases 
a<-cases/(all_cases)


cases <- esoph %>% filter(alcgp == "120+") %>%
  summarize(cases = sum(ncontrols) ) %>%
  .$cases
o<-cases/(all_controls)
a/o

cases <- esoph %>% filter(alcgp == "120+" & tobgp == "30+" ) %>%
  summarize(cases = sum(ncontrols) ) %>%
  .$cases
cases/(all_controls)

cases <- esoph %>% filter(alcgp == "120+" | tobgp == "30+" ) %>%
  summarize(cases = sum(ncontrols) ) %>%
  .$cases
o<-cases/(all_controls)
a/o

###2 continuous distribution
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches

# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

#Plotting the probability density for the normal distribution
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

#Generating normally distributed random numbers
# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

#Monte Carlo simulation of tallest person over 7 feet
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12)    # proportion of times that tallest person exceeded 7 feet (84 inches)

#assessment
set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000,20.9,5.7)
mean(act_scores)
sd(act_scores)
sum(act_scores>=36)
mean(act_scores>=30)
mean(act_scores<=10)
x <- seq(1:36)
f_x <- dnorm(x,20.9,5.7)
plot(x,f_x,type="l")
z_scores <- (act_scores - mean(act_scores)) / sd(act_scores)
mean(z_scores>=2)
2 * sd(act_scores) + mean(act_scores)
qnorm(.975,mean(act_scores),sd(act_scores))
qnorm(.95,mean(act_scores),sd(act_scores))
qnorm(.95,20.9,5.7)
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores,p)
sum(sample_quantiles <= 26)
theoretical_quantiles <- qnorm(p,20.9,5.7)
qqplot(sample_quantiles,theoretical_quantiles,add.line=TRUE)

##3 Modeling a random variable
# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)

#Monte Carlo simulation: Chance of casino losing money on roulette
# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

#Monte Carlo simulation
n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
  sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money

#plot against normal distribution
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

#assessment
.8*-.25 + .2*1
se <- sqrt(44)*abs(1-(-.25))*sqrt(.2*.8)
1 - pnorm(8,0,se)
set.seed(21, sample.kind = "Rounding") 
X <- replicate(10000, sum(sample(c(-.25,1), 44, replace = TRUE, prob = c(.8, .2))))
mean(X>8)

p <- seq(0,1,length=100)

avg <- 44* p
se <- sqrt(44)*abs(1)*sqrt(p*(1-p))
X <- pnorm(35,avg,se)
mean(X>0.2)

p<- 5/38
6 * p + (-1)*(1-p)
abs(6-(-1))*sqrt(p*(1-p))
1/sqrt(500) * abs(6-(-1))*sqrt(p*(1-p))
avg <- 500 * (6 * p + (-1)*(1-p))
se <- sqrt(500) * abs(6-(-1))*sqrt(p*(1-p))
pnorm(0,avg,se)


## the big short

#Code: Interest rate sampling model: calculate losses from 100 loans 
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

#Code: Interest rate Monte Carlo simulation: 
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})

#Code: Plotting expected losses in histogram
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")


#Code: Expected value and standard error of the sum of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

#Code: Calculating interest rates for expected value of 0
x = - loss_per_foreclosure*p/(1-p)
x

#Code: Calculating interest rate for 1% probability of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans


#Code: Monte Carlo simulation for 1% probability of losing money
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money

#Code: Expected value with higher default rate and interest rate
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

#Code: Calculating number of loans for desired probability of losing money
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans


#Code: Monte Carlo simulation with known default probability
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)

#Code: Monte Carlo simulation with unknown default probability
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million
data.frame(profit_in_mil = profit/10^6) %>%
  ggplot(aes(profit_in_mil)) +
  geom_histogram(binwidth = 0.6, col = "black")

## assessment
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a vector called `defaults` that contains the default outcomes of `n` loans
defaults <- sample( c(0,1),n,prob=c(1-p_default,p_default), replace = TRUE)

# Generate `S`, the total amount of money lost across all foreclosures. Print the value to the console.
S<-sum(defaults) * loss_per_foreclosure
S

# the variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Generate a list of summed losses 'S'. Replicate the code from the previous exercise over 'B' iterations to generate a list of summed losses for 'n' loans.  Ignore any warnings for now.
S <- replicate (B,{
  defaults <- sample( c(0,1),n,prob=c(1-p_default,p_default), replace = TRUE)
  sum(defaults) * loss_per_foreclosure
})

#histogram
hist(S/1000000)

# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Compute the standard error of the sum of 10,000 loans
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p_default*(1-p_default))

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Assign a variable `x` as the total amount necessary to have an expected outcome of $0

x<- -loss_per_foreclosure*p_default/(1-p_default)

# Convert `x` to a rate, given that the loan amount is $180,000. Print this value to the console.

x/180000

# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Generate a variable `z` using the `qnorm` function
z <- qnorm(0.01)

# Generate a variable `x` using `z`, `p_default`, `loss_per_foreclosure`, and `n`
x <- -loss_per_foreclosure*( n*p_default - z*sqrt(n*p_default*(1-p_default)))/ ( n*(1-p_default) + z*sqrt(n*p_default*(1-p_default)))

# Convert `x` to an interest rate, given that the loan amount is $180,000. Print this value to the console.
x/180000 


### Insurance
data(death_prob)
head(death_prob)

#1a
death_prob$prob[death_prob$sex=='Female' & death_prob$age==50]

#1b
p<-death_prob$prob[death_prob$sex=='Female' & death_prob$age==50]
profit <- p*(-150000) + (1-p)*1150

#1c
sqrt( (-150000-profit)^2*p +  (1150-profit)^2*(1-p) )
  
#1d
m<-1000*profit
m

#1e
se<-sqrt(1000)*sqrt( (-150000-profit)^2*p +  (1150-profit)^2*(1-p) )
se

#1f
pnorm(0,m,se)

#2a
p<-death_prob$prob[death_prob$sex=='Male' & death_prob$age==50]
p

#2b
n <- 1000
a <- -150000
g <- 700000
b = ( g/n - a*p ) / (1-p)
b

#2c
profit = g/n
se<-sqrt(n)*sqrt( (a-profit)^2*p +  (b-profit)^2*(1-p) )
se

#2d 
pnorm(0,g,se)

#3a
p<-.015
n <- 1000
a <- -150000

profit <- p*a + (1-p)*1150
g<-n*profit
g

#3b
se<-sqrt(1000)*sqrt( (a-profit)^2*p +  (1150-profit)^2*(1-p) )
se

#3c
pnorm(0,g,se)     

#3d
pnorm(-1000000,g,se)     

#3e ???
p <- seq(.01, .03, .001)
p <- .01
profit <- p*a + (1-p)*1150
g<-n*profit
se<-sqrt(1000)*sqrt( (a-profit)^2*p +  (1150-profit)^2*(1-p) )
pnorm(0,g,se)     

#3f  ???
p <- seq(.01, .03, .0025)
p <- .03
profit <- p*a + (1-p)*1150
g<-n*profit
se<-sqrt(1000)*sqrt( (a-profit)^2*p +  (1150-profit)^2*(1-p) )
pnorm(-1000000,g,se)     

#4a
set.seed(25, sample.kind = "Rounding")
p <- .015
n <- 1000
a <- -150000
b <- 1150

profits <- sample( c(a,b),n,prob=c(p,1-p), replace = TRUE)
S<-sum(profits) / 10^6
S

#4b
set.seed(27, sample.kind = "Rounding")
p <- .015
n <- 1000
a <- -150000
b <- 1150
B<-10000
S <- replicate (B,{
  profits <- sample( c(a,b),n,prob=c(p,1-p), replace = TRUE)
  sum(profits) 
})
mean(S< (-1000000))


#5a
p <- .015
n <- 1000
a <- -150000


z <- qnorm(0.05)
x <- -a*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
b<-x
b

#5b
e <- p*a+(1-p)*b
e

#5c
g<- n*e
g


#5d
set.seed(28, sample.kind = "Rounding")
B<-10000
S <- replicate (B,{
  profits <- sample( c(a,b),n,prob=c(p,1-p), replace = TRUE)
  sum(profits) 
})
mean(S< 0)

#6a
set.seed(29, sample.kind = "Rounding")
profit <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(a, b), n, 
                   prob=c(new_p, 1-new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -1000000)    # probability of losing over $10 million
data.frame(profit_in_mil = profit/10^6) %>%
  ggplot(aes(profit_in_mil)) +
  geom_histogram(binwidth = 0.6, col = "black")

