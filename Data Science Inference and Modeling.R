
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


#Code: Function for taking a random draw from a specific urn
take_poll(25)    # draw 25 beads


##Assessment 1
#Exercise 5
# `N` represents the number of people polled
N <- 25

# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p <- seq(0,1,length=100)

# Create a variable `se` that contains the standard error of each sample average
se <- sqrt(p*(1-p)/25)

# Plot `p` on the x-axis and `se` on the y-axis

plot(p,se)


#Exercise 6
# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.

for (N in sample_sizes) {
  se <- sqrt(p*(1-p)/N)
  plot(p,se,ylim=c(0,0.1))
}


#Exercise 9
# `N` represents the number of people polled
N <- 25

# `p` represents the proportion of Democratic voters
p <- 0.45

# Calculate the standard error of the spread. Print this value to the console.

2*sqrt(p*(1-p)/N)

#Code: Computing the probability of  being within .01 of 
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
pnorm(0.01/se) - pnorm(-0.01/se)

#Code: Monte Carlo simulation using a set value of p
p <- 0.45    # unknown p to estimate
N <- 1000

# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)

# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})

#Code: Histogram and QQ-plot of Monte Carlo results
library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)

#Code: Plotting margin of error in an extremely large poll over a range of values of p
library(tidyverse)
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()


#Assessment

#Exercise 1
# Write a function called `take_sample` that takes `p` and `N` as arguements and returns the average value of a randomly sampled population.

take_sample <- function (p,N) {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
}

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Call the `take_sample` function to determine the sample average of `N` randomly selected people from a population containing a proportion of Democrats equal to `p`. Print this value to the console.

take_sample(p,N)

#Exercise 2
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Create an objected called `errors` that replicates subtracting the result of the `take_sample` function from `p` for `B` replications

errors <- replicate(B,{
  p-take_sample(p,N)
})

# Calculate the mean of the errors. Print this value to the console.

mean(errors)

#Exercise 3
hist(errors)

#exercise 4

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the mean of the absolute value of each simulated error. Print this value to the console.

mean(abs(errors))

#Exercise 5
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the standard deviation of `errors`

sqrt(mean(errors^2))

#exercise 6
# Define `p` as the expected value equal to 0.45
p <- 0.45

# Define `N` as the sample size
N <- 100

# Calculate the standard error

sqrt(p*(1-p)/N)

#Exercise 7
# Define `p` as a proportion of Democratic voters to simulate
p <- 0.45

# Define `N` as the sample size
N <- 100

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `X` as a random sample of `N` voters with a probability of picking a Democrat ('1') equal to `p`
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))

# Define `X_bar` as the average sampled proportion

X_bar=mean(X)

# Calculate the standard error of the estimate. Print the result to the console.

sqrt(X_bar*(1-X_bar)/N)

#exercise 8
N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(N,se)


#Exercise 11
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Generate a qq-plot of `errors` with a qq-line showing a normal distribution


qqnorm(errors) 
qqline(errors)


#exercise 12
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Calculate the probability that the estimated proportion of Democrats in the population is greater than 0.5. Print this value to the console.

1 - pnorm(0.5,p,sqrt(p*(1-p)/N))

#exercise 13:
# Define `N` as the number of people polled
N <-100

# Define `X_hat` as the sample average
X_hat <- 0.51

# Define `se_hat` as the standard error of the sample average

se_hat <- sqrt(X_hat*(1-X_hat)/N)

# Calculate the probability that the error is 0.01 or larger

1-(pnorm(X_hat+0.01,X_hat,se_hat)- pnorm(x_hat-0.01,X_hat,se_hat))


#Section 3
data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")


#Code: Monte Carlo simulation of confidence intervals
p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean

#Code: Solving for  with qnorm
z <- qnorm(0.995)    # calculate z to solve for 99% confidence interval
pnorm(qnorm(0.995))    # demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(1-0.995))    # demonstrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)    # demonstrating that this z value gives correct probability for interval

#Code: Monte Carlo simulation
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

#Exercise 1
library(dslabs)
data("polls_us_election_2016")

# Load the data
data(polls_us_election_2016)

# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States

polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-31" & state=="U.S.")

# How many rows does `polls` contain? Print this value to the console.
nrow(polls)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.

N<-polls$samplesize[1]
N

# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat <- polls$rawpoll_clinton[1]/100
X_hat

# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.

se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat

# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.

ci <- c(qnorm(0.025,X_hat,se_hat),  qnorm(0.975,X_hat,se_hat))

#Exercise 2

pollster_results <- polls %>% mutate(X_hat = rawpoll_clinton/100,
                                     se_hat = sqrt(X_hat*(1-X_hat)/samplesize),
                                      lower = qnorm(0.025,X_hat,se_hat),
                                      upper =qnorm(0.975,X_hat,se_hat)) %>%
  select(pollster,enddate,X_hat,se_hat,lower,upper)

head(pollster_results)

#Exercise 3

avg_hit <- pollster_results %>% 
  mutate(hit = lower <= 0.482 & 0.482 <= upper) %>%
  summarise(avg_hit=mean(hit)) 
avg_hit

#Exercise 4

# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") %>%
  mutate(d_hat=(rawpoll_clinton-rawpoll_trump)/100)


# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.

N<-polls$samplesize[1]
N

# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.

d_hat <- polls$d_hat[1]
d_hat
# Assign proportion of votes for Clinton to the variable `X_hat`.

X_hat <- (polls$d_hat[1]+1)/2

# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N) #because d=2p-1 and stddev is linear
se_hat


# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.

ci <- c(qnorm(0.025,d_hat,se_hat),  qnorm(0.975,d_hat,se_hat))


qnorm(0.025,d_hat,se_hat)
qnorm(0.975,d_hat,se_hat)

#Exercise 6
head(polls)
pollster_results <- polls %>% mutate(X_hat = (d_hat+1)/2,
                                     se_hat = 2*sqrt(X_hat*(1-X_hat)/samplesize),
                                     lower = qnorm(0.025,d_hat,se_hat),
                                     upper =qnorm(0.975,d_hat,se_hat)) %>%
  select(pollster,enddate,d_hat,lower,upper)

head(pollster_results)

#Exercise 7
avg_hit <- pollster_results %>% 
  mutate(hit = lower <= 0.021 & 0.021 <= upper) %>%
  summarise(avg_hit=mean(hit)) 
avg_hit

#Exercise 8

polls %>% mutate(error = d_hat -0.021) %>%
  ggplot(aes(x=pollster,y=error)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Exercise 9

polls %>% mutate(error = d_hat -0.021) %>%
  group_by(pollster) %>% filter(n() >= 5) %>%
  ggplot(aes(x=pollster,y=error)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Section 4

#Code: Simulating polls
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

#Code: Calculating the spread of combined polls
d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg

p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
round(d_hat*100,1)
round(moe*100, 1)


#Code: Generating simulated poll data
library(dslabs)
data(polls_us_election_2016)
names(polls_us_election_2016)

# keep only national polls from week before election with a grade considered reliable
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))

# add spread estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# compute estimated spread for combined polls
d_hat <- polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  .$d_hat

# compute margin of error
p_hat <- (d_hat+1)/2
moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))

# histogram of the spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)

#Code: Investigating poll data and pollster bias
# number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())

# plot results by pollsters with at least 6 polls
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))

#Code: Data drive model

# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()

# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)


#Exercise 1
# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Calculate the population average. Print this value to the console.
mean(x)

# Calculate the population standard deviation. Print this value to the console.
sd(x)

#Exercise 2
# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x,N, replace=TRUE)

# Calculate the sample average. Print this value to the console.
mean(X)

# Calculate the sample standard deviation. Print this value to the console.
sd(X)

#Exercise 4
# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)

# Define `se` as the standard error of the estimate. Print this value to the console.
se <- sd(X)/sqrt(N)
se


# Construct a 95% confidence interval for the population average based on our sample. Save the lower and then the upper confidence interval to a variable called `ci`.

ci <- mean(X) + c(qnorm(0.025)*se, qnorm(0.975)*se)
ci

#exercise 5
# Define `mu` as the population average
mu <- mean(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated intervals that contain mu

res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  interval <- mean(X)+ sd(X)/sqrt(N) * c(qnorm(0.025), qnorm(0.975))
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean(res)

#Exercise 6
# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

# Make a boxplot with points of the spread for each pollster

polls %>% ggplot(aes(x=pollster,y=spread)) +
  geom_boxplot()+
  geom_point()


#Exercise 15
# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread

sigma <- polls %>% group_by(pollster) %>%  summarize(s=sd(spread)) %>%
  select(pollster,s)

# Print the contents of sigma to the console

sigma


Exercise 8
# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.

res <- polls %>% group_by(pollster) %>% summarize(a=mean(spread), s=sd(spread),n=length(spread))


# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.

estimate <- res$a[2] - res$a[1] 


# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.

se_hat <- sqrt( (res$s[2]^2)/res$n[2] + (res$s[1]^2)/res$n[1] ) 
se_hat

# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.

ci=estimate+se_hat*c(qnorm(0.025),qnorm(0.975))
ci

#Exercise 16
# We made an object `res` to summarize the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 

# The variables `estimate` and `se_hat` contain the spread estimates and standard error, respectively.
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])

# Calculate the p-value

(1- pnorm(estimate/se_hat,0,1 ))*2


#Exercise 17
# Execute the following lines of code to filter the polling data and calculate the spread
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

# Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation. Print the contents of this object to the console.

var <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread)) 

var

##section 5
#Code: Monte Carlo simulation
prev <- 0.00025    # disease prevalence
N <- 100000    # number of tests
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))

N_D <- sum(outcome == "Disease")    # number with disease
N_H <- sum(outcome == "Healthy")    # number healthy

# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))

table(outcome, test)


# Assessment

#Exercise 2
# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Calculate the probability of both sons dying of SIDS. Print this value to the console.

Pr_2 * Pr_1

#Exercise 4
# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Define `Pr_B` as the probability of both sons dying of SIDS
Pr_B <- Pr_1*Pr_2

# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000

# Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_BA <- 0.50

# Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.

Pr_AB <- Pr_BA * Pr_A / Pr_B
Pr_AB

#Exercise 6

# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`). Print the results to the console.
#results <- polls %>% group_by(pollster) %>% summarize(avg=mean(spread), se=sd(spread) /sqrt(n() ))
#results
results <- polls %>% summarize(avg = mean(spread),  se = sd(spread)/sqrt(n()))
results


#exercise 8
# The results` object has already been loaded. Examine the values stored: `avg` and `se` of the spread
results

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results`
sigma <-results$se

# Define a variable called `Y` that contains the average in the object `results`
Y <- results$avg

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B <- sigma^2 /(sigma^2+tau^2)
B 

# Calculate the expected value of the posterior distribution
B * mu + (1-B)*Y

#exercise 9
# Here are the variables we have defined
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

# Compute the standard error of the posterior distribution. Print this value to the console.

sqrt(1 / (1/tau^2 + 1/sigma^2 ))

#exercise 10
# Here are the variables we have defined in previous exercises
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
erw <- B * mu + (1-B)*Y
ci <- erw + se * c(qnorm(0.025),qnorm(0.975))

#exercise 11
# Assign the expected value of the posterior distribution to the variable `exp_value`
exp_value <- B*mu + (1-B)*Y 

# Assign the standard error of the posterior distribution to the variable `se`
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.

pnorm(0,exp_value, se)

exercise 12

# Define the variables from previous exercises
mu <- 0
sigma <- results$se
Y <- results$avg

# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)

# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0

p_calc <- function(tau){
  B <- sigma^2 / (sigma^2 + tau^2)
  se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
  erw <- B * mu + (1-B)*Y
  pnorm(0,erw, se)
}



# Create a vector called `ps` by applying the function `p_calc` across values in `taus`

ps <- p_calc(taus)

# Plot `taus` on the x-axis and `ps` on the y-axis

plot(taus,ps)


##Section 6

#Code: Definition of results object
library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)

#Code: Computing the posterior mean, standard error, credible interval and probability

mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se

# 95% credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

# probability of d > 0
1 - pnorm(0, posterior_mean, posterior_se)

#Code: Simulated data with 6 pollsters
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))

#Code: Simulated data with house bias
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

#Code: Simulated data with house bias and general bias
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I, 0, 0.025)    # assume standard error of pollster-to-pollster variability is 0.025
X <- sapply(1:I, function(i){
  d + h[i] + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

#Code: Calculating probability of  with general bias
mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

1 - pnorm(0, posterior_mean, posterior_se)

#Code: Top 5 states ranked by electoral votes
#The results_us_election_2016 object is defined in the dslabs package:
  
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)
#five states with biggest electoral votes
results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

#Code: Computing the average and standard deviation for each state
results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", state) &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

# 10 closest races = battleground states
results %>% arrange(abs(avg))

# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")

# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

#Code: Calculating the posterior mean and posterior standard error
mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))
#Code: Monte Carlo simulation of Election Night results (no general bias)
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election

# histogram of outcomes
data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)



#Code: Monte Carlo simulation including general bias
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election

# histogram of outcomes
data.frame(clinton_EV_2) %>%
  ggplot(aes(clinton_EV_2)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

#Code: Variability across one pollster
# select all national polls by one pollster
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color = "black")

#Code: Trend across time for several pollsters
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)
#Code: Plotting raw percentages across time
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))


#exercise 1
# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that has the columns indicated in the instructions

cis <- polls %>% mutate(X_hat = (spread+1)/2,
                                     se_hat = 2*sqrt(X_hat*(1-X_hat)/samplesize),
                                     lower = qnorm(0.025,spread,se_hat),
                                     upper =qnorm(0.975,spread,se_hat)) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

cis <- polls %>% mutate(X_hat = (spread+1)/2,
                        se_hat = 2*sqrt(X_hat*(1-X_hat)/samplesize),
                        lower = spread+se_hat*qnorm(0.025),
                        upper =qnorm(0.975,spread,se_hat)) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)
head(cis)

#exercise 2
# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. Print this object to the console.
p_hits<- ci_data %>% mutate(hit=actual_spread>=lower&actual_spread<=upper) %>%
  summarize(p_hits=mean(hit)) 

#exercise 3
# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls.

p_hits<- ci_data %>% mutate(hit=actual_spread>=lower&actual_spread<=upper) %>% group_by(pollster) %>%
  filter(n()>=5) %>%
  summarize(proportion_hits=mean(hit), n=n(), grade=first(grade)) %>%
  arrange(desc(proportion_hits))
p_hits

#exercise 4
# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls.
p_hits<- ci_data %>% mutate(hit=actual_spread>=lower&actual_spread<=upper) %>% group_by(state) %>%
  filter(n()>=5) %>%
  summarize(proportion_hits=mean(hit), n=n()) %>%
  arrange(desc(proportion_hits))


#exercise 5
# The `p_hits` data have already been loaded for you. Use the `head` function to examine it.
head(p_hits)

# Make a barplot of the proportion of hits for each state

p_hits %>% arrange(proportion_hits) %>%
  ggplot(aes(x=state,y=proportion_hits)) +
  geom_bar(stat="identity") +
  coord_flip()

#exercise 6
# The `cis` data have already been loaded. Examine it using the `head` function.
head(cis)

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted

errors <- cis %>% mutate(error=spread - actual_spread, hit = sign(spread)==sign(actual_spread))

# Examine the last 6 rows of `errors`

tail(errors, 6)


#exercise 7

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has 5 or more polls

p_hits <- errors %>% group_by(state) %>%
  filter( n() >= 5) %>%
  summarize(proportion_hits=mean(hit), n=n()) 

# Make a barplot of the proportion of hits for each state

p_hits %>% ggplot(aes(x=state, y=proportion_hits)) +
  geom_bar(stat="identity") +
  coord_flip()

#exercise 8
# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate a histogram of the error

hits <- errors %>% group_by(state) %>%
  summarize(hits=sum(hit))
hist(hits$hits)
# Calculate the median of the errors. Print this value to the console.
median(errors$error)

#exercise 9
errors %>% filter(grade %in% c("A+", "A", "A-", "B+")) %>%
  group_by(state) %>%
  filter(n()>=5) %>%
  ungroup() %>%
  arrange(state,error) %>%
  ggplot(aes(x=state,y=error)) +
  geom_boxplot()+
  geom_point()

##section 6
Code: Calculating 95% confidence intervals with the t-distribution
z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

# quantile from t-distribution versus normal distribution
qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
qnorm(0.975)


#exercise 1
# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'.

1-pt(2,3)+pt(-2,3)

#exercise 2
# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df <- 3:50

# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 

pt_func <- function(df){1-pt(2,df)+pt(-2,df)}

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities

probs <- sapply(df,pt_func)

# Plot 'df' on the x-axis and 'probs' on the y-axis

plot(df,probs)


#exercise 3
# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations

res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  interval <- mean(X) + c(-1,1)*qnorm(0.975)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.

mean(res)

#exercise 4
# The vector of filtered heights 'x' has already been loaded for you. Calculate the mean.
mu <- mean(x)

# Use the same sampling parameters as in the previous exercise.
set.seed(1)
N <- 15
B <- 10000

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution

res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  interval <- mean(X) + c(-1,1)*qt(0.975,N-1)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.

mean(res)


##section 7
#Code: Research funding rates example
# load and inspect research funding rates object
library(tidyverse)
library(dslabs)
data(research_funding_rates)
research_funding_rates

# compute totals that were successful or not successful
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))
#Code: Two-by-two table and p-value for the Lady Tasting Tea problem
tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab

# p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")

#Code: Chi-squared test
# compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate

# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# compute null hypothesis two-by-two table
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>%
  chisq.test()
chisq_test$p.value


#Code: Odds ratio
# odds of getting funding for men
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women
#Code: p-value and odds ratio responses to increasing sample size
# multiplying all observations by 10 decreases p-value without changing odds ratio
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()
  
#exercise 1
# The 'errors' data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
#Totals ist falsch pivotiert!!!
totals <- errors %>% filter(grade %in% c("A-","C-")) %>%
  group_by(grade) %>%
  summarize( hits=sum(hit), misses=sum(!hit))


# Print the proportion of hits for grade A- polls to the console
totals$hits[totals$grade=="A-"]/(totals$hits[totals$grade=="A-"]+totals$misses[totals$grade=="A-"])

# Print the proportion of hits for grade C- polls to the console
totals$hits[totals$grade=="C-"]/(totals$hits[totals$grade=="C-"]+totals$misses[totals$grade=="C-"])


#exercise 2

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.

chisq_test <- totals %>% select(-hit) %>% chisq.test()

# Print the p-value of the chi-squared test to the console

chisq_test$p.value 

#exercise 3
# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls

odds_C <- totals$`C-`[totals$hit] / totals$`C-`[!totals$hit]


# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls

odds_A <- totals$`A-`[totals$hit] / totals$`A-`[!totals$hit]


# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls

odds_A/odds_C




##BREXIT

# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

#Final Brexit parameters
#Define  as the actual percent voting "Remain" on the Brexit referendum and  as the actual spread of the Brexit referendum with "Remain" defined as the positive outcome:
  
p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

#question 1
N <- 1500
e<-N*p
e
se <- sqrt(p*(1-p)*N)
se
e
se <- sqrt(p*(1-p)/N)
se
d
se <-2*sqrt(p*(1-p)/N)

#question 2
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)
mean(brexit_polls$spread)
sd(brexit_polls$spread)
mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)

#question 3
brexit_polls[1,]
x_hat<-brexit_polls$x_hat[1]
se_hat <- sqrt(x_hat*(1-x_hat)/brexit_polls$samplesize[1])
se_hat
 c(qnorm(0.025,x_hat,se_hat),  qnorm(0.975,x_hat,se_hat))
 
#question 4
 
# suggested libraries
library(tidyverse)
 
# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
   mutate(x_hat = (spread + 1)/2)
 
# final proportion voting "Remain"
p <- 0.481
d <- 2*p-1  
 
june_polls<- brexit_polls %>% 
   filter(enddate >= "2016-06-01") %>%
   mutate(se_x_hat=sqrt(x_hat*(1-x_hat)/samplesize),
          spread_se=2*se_x_hat,
          lower=spread+spread_se*qnorm(0.025),
          upper=spread+spread_se*qnorm(0.975),
          hit= d>=lower & d<=upper)
head(june_polls) 
nrow(june_polls)
mean(june_polls$lower<=0&june_polls$upper>=0)
mean(june_polls$lower>=0)
mean(june_polls$hit)

#question #5
june_polls %>% group_by(pollster) %>%
  summarize(hits=mean(hit), n=n()) %>%
  arrange(hits)


#question #6
june_polls %>% ggplot(aes(x=poll_type,y=spread))+
  geom_boxplot()

#question #7
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
  spread = sum(spread*samplesize)/N,
  p_hat = (spread + 1)/2,
  se_p_hat=sqrt(p_hat*(1-p_hat)/N),
  se_spread=2*se_p_hat,
  lower=spread+se_spread*qnorm(0.025),
  upper=spread+se_spread*qnorm(0.975),
  hit= d>=lower & d<=upper)
            
combined_by_type          

#question #9
# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

brexit_hit %>% group_by(poll_type) %>% 
  summarize(hits=sum(hit), miss=sum(!hit)) %>%
  select(-poll_type) %>% chisq.test()

#question 10
tbt <- brexit_hit %>% group_by(poll_type) %>% 
  summarize(hits=sum(hit), miss=sum(!hit))

tbt$hits/tbt$miss

(tbt$hits[1]/tbt$miss[1]) / (tbt$hits[2]/tbt$miss[2])


#question 11
brexit_polls %>% ggplot(aes(x=enddate,y=spread,color = poll_type))+
  geom_smooth(method="loess", span = 0.4) +
  geom_point()+
  geom_hline(yintercept=d)


#question 12
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
head(brexit_long)

brexit_long %>% ggplot(aes(x=enddate,y=proportion,color = vote))+
                         geom_smooth(method="loess", span = 0.3)                 
