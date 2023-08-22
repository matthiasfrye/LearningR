library(tidyverse)
library(dslabs)
library(dplyr)

##Basics

#use dataset murders
data(murders)

#chart murders
murders %>% ggplot(aes(population, total, label=abb, color=region)) + geom_label()

#play wuith variables
a <- 1
b <- 1
c <- -1
print(a)

#some functions
ls() #variables
help("log") #help
args(log) #help
?"+" #help
class(ls) #data type

#data sets
data() #all data sets
class(murders) 
str(murders) #structure
head(murders) #first lines
murders$population 
names(murders)
pop <- murders$population
length(pop)
length(murders$state)
class(pop)

#data types
z <- 2==3
class(z)
class(murders$region)
levels(murders$region)
a<-2
b<--1
c<-4

#calculation
(-b+sqrt(b*b-4*a*c))/(2*a)
(-b-sqrt(b*b-4*a*c))/(2*a)

##Data
#create a vector
v<- c("a","b","c")
codes <- c("italy" = 380, "canada" = 124, "egypt" = 818) # named elements
class(codes)

# We can also name the elements of a numeric vector using the names() function
codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country
ind <- c(FALSE, TRUE, TRUE)
codes[ind] #where  ind is TRUE

#sequence
seq (1, 10, 2)
codes[2]
codes[c(1,3)] #element1 and 3
codes[1:2] # elements 1 to 2
codes[c("egypt","italy")] # access using names

#coercion
x <- c("a", 1)
class(x)

#conversion
x <- 1:5
y <- as.character(x)
z <- as.numeric(y)
e<-as.numeric(c("1","2","c","s")) #introduce NA missing value
is.na(e) #check for NA
sum(is.na(e)) # Determine how many NA ind has using the sum function

#sorting
sort(murders$total) # sortierung
index <- order(murders$total) #index der Sortierung
murders$state[index] #Staaten anzeigen
max(murders$total) #Max
murders$state[which.max(murders$total)] #Max Index
rank(c(31, 4, 15, 92, 65)) #Rang (kleinste zuerst)

#vector arithmetic
murder_rate <- murders$total / murders$population * 100000
murders$state[order(murder_rate, decreasing=TRUE)]

name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
time <- time/60
speed <- distance/time
time[4]
speed[1]
speed

## Indexing
murder_rate <- murders$total / murders$population * 100000
index <- murder_rate <= 0.71 #logical index
murders$state[index] #index with logical vector
sum(index) # coercion logical to numeric
west <- murders$region == "West"
safe <- murder_rate <= 1
murders$state[west & safe] #index with logical vector

# to determine the murder rate in Massachusetts we may do the following
index <- which(murders$state == "Massachusetts")
index
murder_rate[index]

# to obtain the indices and subsequent murder rates of New York, Florida, Texas, we do:
index <- match(c("New York", "Florida", "Texas"), murders$state)
index
murders$state[index]
murder_rate[index]

# to see if Boston, Dakota, and Washington are states
c("Boston", "Dakota", "Washington") %in% murders$state

# adding a column with mutate
murders <- mutate(murders, rate = total / population * 100000)

# subsetting with filter
filter(murders, rate <= 0.71)

# selecting columns with select
new_table <- select(murders, state, region, rate)

# using the pipe
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)

# creating a data frame with stringAsFactors = FALSE
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90))
str(grades)
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = TRUE)
str(grades)
nrow(grades)

##Plotting

# a simple scatterplot of total murders versus population
x <- murders$population /10^6
y <- murders$total
plot(x, y)

# a histogram of murder rates
murders <- mutate(murders, rate = total / population * 100000)
hist(murders$rate)
murders$state[which.max(murders$rate)]

# boxplots of murder rates by region
boxplot(rate~region, data = murders)

#summarize
# minimum, median, and maximum murder rate for the states in the West region
s <- murders %>% 
  filter(region == "West") %>%
  summarize(minimum = min(rate), 
            median = median(rate), 
            maximum = max(rate))
s

# average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate

# returning minimum, median, and maximum as a data frame
my_quantile <- function(x){
  r <-  quantile(x, c(0, 0.5, 1))
  data.frame(minimum = r[1], median = r[2], maximum = r[3]) 
}
murders %>% 
  filter(region == "West") %>%
  summarize(my_quantile(rate))

# using pull to save the number directly
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  pull(rate)

# using the dot to access the rate
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  .$rate

# summarize after grouping
murders %>% 
  group_by(region) %>%
  summarize(median = median(rate))

# order the states by murder rate in descending order
murders %>% arrange(desc(rate)) %>% head()

# order the states by region and then by murder rate within region
murders %>% arrange(region, rate) %>% head()

# return the top 10 states ranked by murder rate, sorted by murder rate
murders %>% arrange(desc(rate)) %>% top_n(10)

##Assessment
library(dslabs)
data(heights)
options(digits = 3) 

str(heights)
average <- mean(heights$height)
ind<-heights$height>average
sum(ind)
ind<-heights$height>average & heights$sex=="Female"
sum(ind)
ind<-heights$sex=="Female"
sum(ind)/length(heights$sex)
minimum <- min(heights$height)
minimum
match(minimum,heights$height)
heights$sex[match(minimum,heights$height)]
maximum <- max(heights$height)
maximum
integers <- seq(1:100)
ind<-integers <= maximum & minimum <= integers
x<-integers[ind]
sum(!(x %in% heights$height))
heights <- mutate(heights,ht_cm=height*2.54)
heights$ht_cm[18]
mean(heights$ht_cm)
females <- heights %>% filter(sex=="Female")
lengths(females)
mean(females$ht_cm)
library(dslabs)
data(olive)
head(olive)
plot(olive$palmitic,olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(palmitic~region,data=olive)


##Programming

#if
a <- 2 #0
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}

murder_rate <- murders$total / murders$population*100000
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
  print(murders$state[ind]) 
} else{
  print("No state has murder rate that low")
}


# the ifelse() function is particularly useful on vectors
a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA)

# the ifelse() function is also helpful for replacing missing values
data(na_example)
sum(is.na(na_example))
no_nas <- ifelse(is.na(na_example), 0, na_example) 
sum(is.na(no_nas))

# the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z) #like OR
all(z) #like AND

# example of defining a function to compute the average of a vector x
avg <- function(x=c(0,0)){ #default with vector 0,0
  s <- sum(x)
  n <- length(x)
  s/n # return value in last line
}
avg(seq(1,100))

#loops
# creating a function that computes the sum of integers 1 through n
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}

# a for-loop for our summation
m <- 25
s_n <- vector(length = m) # create an empty vector
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}

# creating a plot for our summation function
n <- 1:m
# overlaying our function with the summation formula
plot(n, s_n)
lines(n, n*(n+1)/2)

#assessment
library(dslabs)
data(heights)
sum(ifelse(heights$sex=="Female",1,2))
mean(ifelse(heights$height>72,heights$height,0))
inches_to_ft<-function(x){x/12} 
inches_to_ft(144)
sum(inches_to_ft(heights$height)<5)
