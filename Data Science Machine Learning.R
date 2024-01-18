rm(list=ls())
library(tidyverse) #load also ggplot2
library(dslabs)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(titanic)
library(gtools)
library(rvest)
library(purrr)
library(lubridate)
library(tidyr)
library(scales)
library(tidytext)
library(broom)
library(caret)
library(gam)
library(Rborist)
library(randomForest)
library(naivebayes)
library(doParallel)
library(rethnicity)


library(mosaic) # Diesen Befehl bei jeder Session am Anfang ausführen
library(readr)
library(gplots)
library(readxl)



## Sex predicted by height
data(heights)

# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

# compute accuracy
mean(y_hat == test_set$sex)

# compare heights in males and females in our data set
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))

# now try predicting "male" if the height is within 2 SD of the average male
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

# examine the accuracy of 50 cutoffs
cutoff <- seq(61, 70, 0.2)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
#plot accuracy
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

#find best cutoff
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

#test cutoff with test data set
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

#alternative model using linear regression
train_set$sex_num <- ifelse(train_set$sex=="Male", 1,0)
mod<-lm(sex_num~height,data=train_set)
mod

#if predicted value is biggr than 0.5
y_hat <- ifelse(predict(mod, data.frame(height=test_set$height))  >= 0.5, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

#exercise
mnist <- read_mnist()
str(mnist)

##accuracy
# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))
prev <- mean(y == "Male")

confusionMatrix(data = y_hat, reference = test_set$sex)

# get the metrics
cm <- confusionMatrix(data = y_hat, reference = test_set$sex)

# access specific metrics
cm$overall["Accuracy"]

cm$byClass[c("Sensitivity","Specificity", "Prevalence")]


# maximize F-score
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>% 
  ggplot(aes(cutoff, F_1)) + 
  geom_point() + 
  geom_line()

max(F_1)

best_cutoff_2 <- cutoff[which.max(F_1)]
best_cutoff_2

y_hat <- ifelse(test_set$height > best_cutoff_2, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)


#ROC curves
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()


##exercise

library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

mean(y[x=="inclass"]=="Female")
mean(y[x=="online"]=="Female")

y_hat <- ifelse(x=="online", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_hat == y)
table(y_hat, y)
sensitivity(data = y_hat, reference = y)
specificity(data = y_hat, reference = y)
mean(y=="Female")


library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(76)
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# accuracy of Sepal.length
mean(train$Sepal.Length~train$Species)
min(train$Sepal.Length)
max(train$Sepal.Length)

# examine the accuracy of 50 cutoffs
cutoff <- seq(4.9, 7.9, 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
#plot accuracy
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

# accuracy of Sepal.Width
mean(train$Sepal.Width~train$Species)
min(train$Sepal.Width)
max(train$Sepal.Width)

# examine the accuracy of 50 cutoffs
cutoff <- seq(2, 3.8, 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
#plot accuracy
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

# accuracy of Petal.Length
mean(train$Petal.Length~train$Species)
min(train$Petal.Length)
max(train$Petal.Length)

# examine the accuracy of 50 cutoffs
cutoff <- seq(3.3, 6.7, 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
#plot accuracy
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)
#find best cutoff
best_length_cutoff <- cutoff[which.max(accuracy)]
best_length_cutoff


# accuracy of Petal.Width
mean(train$Petal.Width~train$Species)
min(train$Petal.Width)
max(train$Petal.Width)

# examine the accuracy of 50 cutoffs
cutoff <- seq(1, 2.5, 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
#plot accuracy
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

#accuracy in test data
#find best cutoff
best_width_cutoff <- cutoff[which.max(accuracy)]
best_width_cutoff

#predict test data with Petal.Width
y_hat <- ifelse(test$Petal.Width > best_width_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)


#predict ttest data with Petal.Width and with Petal.Length
y_hat <- ifelse(test$Petal.Width > best_width_cutoff & test$Petal.Length > best_length_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)


plot(iris, pch=21, bg=iris$Species)

##Exercise

# Q1

# P( d ) = 0.02
# P( t+ | d ) = 0.85
# P( t- | h ) = 0.9    => P( t+ | h ) = 0.1
# P( t+ ) = P( t+ | d )* P( d ) + P( t+ | h ) *P( h )
0.85 * 0.02 + 0.1 *0.98
# = 0.115


# P( d | t+ ) = P( t+ | d ) * P( d )/ P( t+ )
 0.85 * 0.02 / 0.115
# = 0.1478261

#Q2-Q4 
set.seed(1) 
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

# Q2
mean(test) 

# Q3
# sample data
mean(disease[test==0])

#theoretical solution
# P( d | t- ) = P( t- | d ) * P( d ) / P( t- ) 
(1-0.85) * 0.02 / (1-0.115)

#Q4
# sample data
mean(disease[test==1])

#Q5

mean(disease[test==1]) / 0.02

#Q6

library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

#Q7
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#Q8
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)


## Case study 7 or 2
# load the dataset
library(tidyverse)
library(dslabs)
data("mnist_27")

# explore the data by plotting the two predictors
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

# smallest and largest values of x1 and x2
if(!exists("mnist")) mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%  
    mutate(label=titles[i],  
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
p1 <- tmp %>% ggplot(aes(Row, Column, fill=value)) + 
  geom_raster(show.legend = FALSE) + 
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) + 
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5) +
  ggtitle("Largest and smallest x_1")

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%  
    mutate(label=titles[i],  
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
p2 <- tmp %>% ggplot(aes(Row, Column, fill=value)) + 
  geom_raster(show.legend = FALSE) + 
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) + 
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5) +
  ggtitle("Largest and smallest x_2")
gridExtra::grid.arrange(p1, p2, ncol = 2)

# fit the model
fit <- mnist_27$train %>%
  mutate(y = ifelse(y == 7, 1, 0)) %>%
  lm(y ~ x_1 + x_2, data = .)

# build a decision rule
library(caret)

p_hat <- predict(fit, newdata = mnist_27$test, type = "response")
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))

confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]]

# plot the true values
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z = p, fill = p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")

# visual representation of p_hat
p_hat <- predict(fit, newdata = mnist_27$true_p)
p_hat <- scales::squish(p_hat, c(0, 1))
p1 <- mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 

p2 <- mnist_27$true_p %>% mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") + 
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test) 
gridExtra::grid.arrange(p1, p2, ncol = 2)

## Exercise
# Q1
library(tidyverse)
library(caret)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


set.seed(1)
results <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  test_set <- dat[test_index, ]
  train_set <- dat[-test_index, ]
  
  fit <- train_set %>%
    lm(y ~ x, data = .)
  
  y_hat <- predict(fit, test_set)
  
  RMSE(y_hat,test_set$y)
})
mean(results)
sd(results)


#Q2

test_model <- function(n){

  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
  
  results <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    test_set <- dat[test_index, ]
    train_set <- dat[-test_index, ]
    
    fit <- train_set %>%
      lm(y ~ x, data = .)
    
    y_hat <- predict(fit, test_set)
    
    RMSE(y_hat,test_set$y)
  })
  c(mean(results), sd(results))
}

set.seed(1)
n <- c(100, 500, 1000, 5000, 10000)
sapply(n, test_model)


# Q3
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
results <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  test_set <- dat[test_index, ]
  train_set <- dat[-test_index, ]
  
  fit <- train_set %>%
    lm(y ~ x, data = .)
  
  y_hat <- predict(fit, test_set)
  
  RMSE(y_hat,test_set$y)
})
mean(results)
sd(results)


#Q6 + Q7

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))


set.seed(1)

test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]

fit <- train_set %>%
  lm(y ~ x_1, data = .)
y_hat <- predict(fit, test_set)
RMSE(y_hat,test_set$y)

fit <- train_set %>%
  lm(y ~ x_2, data = .)
y_hat <- predict(fit, test_set)
RMSE(y_hat,test_set$y)

fit <- train_set %>%
  lm(y ~ x_1 + x_2, data = .)
y_hat <- predict(fit, test_set)
RMSE(y_hat,test_set$y)


#Q8

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))


set.seed(1)

test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]

fit <- train_set %>%
  lm(y ~ x_1, data = .)
y_hat <- predict(fit, test_set)
RMSE(y_hat,test_set$y)

fit <- train_set %>%
  lm(y ~ x_2, data = .)
y_hat <- predict(fit, test_set)
RMSE(y_hat,test_set$y)

fit <- train_set %>%
  lm(y ~ x_1 + x_2, data = .)
y_hat <- predict(fit, test_set)
RMSE(y_hat,test_set$y)


#Smoothing
# see that the trend is wobbly
library(tidyverse)
set.seed(1)
n <- 100
x <- seq(-pi*4, pi*4, len = n)
tmp <- data.frame(x = x , f = sin(x) + x/8, e = rnorm(n, 0, 0.5)) 
p1 <- qplot(x, f, main = "smooth trend", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p2 <- qplot(x, e, main = "noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
p3 <- qplot(x, f+e, main = "data = smooth trend + noise", ylim = range(tmp$f+tmp$e), data = tmp, geom = "line")
gridExtra::grid.arrange(p1, p2, p3)

# estimate the time trend in the 2008 US popular vote poll margin
library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008)

# use regression to estimate
resid <- ifelse(lm(margin~day, data = polls_2008)$resid > 0, "+", "-")
polls_2008 %>% 
  mutate(resid = resid) %>% 
  ggplot(aes(day, margin)) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_point(aes(color = resid), size = 3)




# bin smoothers
span <- 3.5
tmp <- polls_2008 %>%
  crossing(center = polls_2008$day) %>%
  mutate(dist = abs(day - center)) %>%
  filter(dist <= span) 

tmp %>% filter(center %in% c(-125, -55)) %>%
  ggplot(aes(day, margin)) +   
  geom_point(data = polls_2008, size = 3, alpha = 0.5, color = "grey") +
  geom_point(size = 2) +    
  geom_smooth(aes(group = center), 
              method = "lm", formula=y~1, se = FALSE) +
  facet_wrap(~center)

# larger span
span <- 7 
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel = "box", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

# kernel
span <- 7
fit <- with(polls_2008, 
            ksmooth(day, margin, kernel = "normal", bandwidth = span))

polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")


polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(color="red", span = 0.15, method = "loess", method.args = list(degree=1))

total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
fit_2 <- loess(margin ~ day, span = span, data=polls_2008)


polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth_1), color="red", lty = 2) +
  geom_line(aes(day, smooth_2), color="orange", lty = 1)


polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth()

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() +
  geom_smooth(color="red",fill="red",alpha=0.2) +
  geom_smooth(method=loess, method.args= list(degree=1, span=0.15),color="blue",fill="blue",alpha=0.2)


## Comprehension Check: Smoothing
# Q1


library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_tibble() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

dat <- dat %>% filter(!is.na(deaths))

span <- 60 / diff(range(as.numeric(dat$date)))
dat %>% ggplot(aes(date, deaths)) +
  geom_point() +
  geom_smooth(method=loess, method.args= list(degree=1, span=span),color="red",size=2)

fit <- loess(deaths ~ as.numeric(date), degree=1, span = span, data=dat)
dat %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(date, deaths)) +
  geom_point() +
  geom_line(aes(date, smooth), color="red",linewidth=2) 

# Q2
dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

#Q3
library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)

fit <- mnist_27$train %>%
  mutate(y = ifelse(y == 7, 1, 0)) %>%
  loess(y ~ x_2, degree=1, data=.)

p_hat <- predict(fit, newdata = mnist_27$test, type = "response")
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))

confusionMatrix(y_hat, mnist_27$test$y)$overall[["Accuracy"]]


## KNN

data("mnist_27")

x <- as.matrix(mnist_27$train)
y <- mnist_27$train$y
knn_fit <- knn3(x,y)


mnist_27$test %>%
  ggplot(aes(x_1, x_2, color = y)) +
  geom_point()

# KNN model with default parameters
# y ~ x_1 + x_2
knn_fit <- knn3(y ~ ., data = mnist_27$train)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

#linear model
fit_lm <- mnist_27$train %>% 
  mutate(y = ifelse(y == 7, 1, 0)) %>% 
  lm(y ~ x_1 + x_2, data = .)
p_hat_lm <- predict(fit_lm, mnist_27$test)
y_hat_lm <- factor(ifelse(p_hat_lm > 0.5, 7, 2))
confusionMatrix(y_hat_lm, mnist_27$test$y)$overall["Accuracy"]

#plot compare tru p with KNN prediction
plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
    stat_contour(breaks=c(0.5), color="black")
}
p1 <- plot_cond_prob() +
  ggtitle("True conditional probability")
p2 <- plot_cond_prob(predict(knn_fit, mnist_27$true_p)[,2]) +
  ggtitle("kNN-5 estimate")
grid.arrange(p2, p1, nrow=1)

# Accurarcy on training vs test data
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn, mnist_27$train$y)$overall["Accuracy"]

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

## Overfitting and oversmoothing

#Accuracy of K=1
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$train$y)$overall["Accuracy"]

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_1, mnist_27$test$y)$overall["Accuracy"]

#visualisation k=1
p1 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$train, aes(x_1, x_2, color= y), pch=21) +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Train set")
p2 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit_1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$test, aes(x_1, x_2, color= y), 
             pch=21, show.legend = FALSE) +
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Test set")
grid.arrange(p1, p2, nrow=1)

# k=401
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn_401, mnist_27$test$y)$overall["Accuracy"]

# visualize against linear regression
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p1 <- plot_cond_prob(predict(fit_glm, mnist_27$true_p)) +
  ggtitle("Regression")
p2 <- plot_cond_prob(predict(knn_fit_401, mnist_27$true_p)[,2]) +
  ggtitle("kNN-401")
grid.arrange(p1, p2, nrow=1)


# Exercise
#Q1

set.seed(1)
y <- heights$sex
x <- heights$height
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

test_model <- function(n){
fit <- knn3(sex ~ height, data = train_set, k = n)
  y_hat <- predict(fit, test_set, type = "class")
  F_meas(data = y_hat, reference = factor(test_set$sex))  
}
  
n <- seq(1, 101, 3)
F_1 <- sapply(n, test_model)
max(F_1)
n[which.max(F_1)]


#Q2
library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1)

set.seed(1)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)


test_model <- function(n){
  fit <- knn3(x[-test_index,], y[-test_index], k = n)
  
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]), type = "class")
  confusionMatrix(y_hat, test_set$y)$overall["Accuracy"]
}

n <- seq(1, 11, 2)
Accu <- sapply(n, test_model)
Accu

## Choosing k

ks <- seq(3, 251, 2)

library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(y_hat, mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(y_hat, mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})

accuracy %>% mutate(k = ks) %>%
  gather(set, accuracy, -k) %>%
  mutate(set = factor(set, levels = c("train", "test"))) %>%
  ggplot(aes(k, accuracy, color = set)) + 
  geom_line() +
  geom_point()

ks[which.max(accuracy$test)]
max(accuracy$test)


#Bootstrap
set.seed(1995)
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income)
m

#sample of 100
N <- 100
X <- sample(income, N)
median(X)

#distribution of Median
library(gridExtra)
B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M), xlab = "theoretical", ylab = "sample") + 
  geom_abline()
grid.arrange(p1, p2, ncol = 2)

# confidence interval 
# compare the 95% CI based on the CLT to the actual one
median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)
quantile(M, c(0.025, 0.975))

# bootstrap and approximate the distribution
B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

# look at the confidence interval from the bootstrap
quantile(M_star, c(0.025, 0.975))


## Exercise
#Q1
library(dslabs)
library(caret)

data(mnist_27)

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)
sum(indexes$Resample01==3)
sum(indexes$Resample01==4)
sum(indexes$Resample01==7)

#Q2
sum(indexes$Resample01==3)+
sum(indexes$Resample02==3)+
sum(indexes$Resample03==3)+
sum(indexes$Resample04==3)+
sum(indexes$Resample05==3)+
sum(indexes$Resample06==3)+
sum(indexes$Resample07==3)+
sum(indexes$Resample08==3)+
sum(indexes$Resample09==3)+
sum(indexes$Resample10==3)


#oder
x=sapply(indexes, function(ind){
  
  sum(ind == 3)
  
})

sum(x)

#Q3

set.seed(1)
B <- 10^5
q <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(q)
sd(q)


#Q4
set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
B <- 10^5
q <- replicate(B, {
  i <- createResample(y, 1)
  z <- y[i$Resample1]
  quantile(z, 0.75)
})
mean(q)
sd(q)

#oder
set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
B <- 10^5
i <- createResample(y, B)
q <- sapply(i, function(ind) {
  z <- y[ind]
  quantile(z, 0.75)
})
mean(q)
sd(q)


## caret package

library(tidyverse)
library(dslabs)
data("mnist_27")

library(caret)

#train models with dfferent methods, with cross validation if needed
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

#predict with same syntax
y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

#get accuracy
confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

# understand model parameters
getModelInfo("knn")
modelLookup("knn")

#plot parameters
ggplot(train_knn, highlight = TRUE)

# parameters as data frame
data.frame(k = seq(9, 67, 2))

#seeding for cross validation (default 25 bootstrap samples with 25% sample size)
set.seed(2008)
train_knn <- train(y ~ ., method = "knn",
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = TRUE)

#access best parameters
train_knn$bestTune
train_knn$finalModel

#get accuracy with test data, predict will use best model
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

#different cross validation
set.seed(2008)
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn",
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

##compare accuracy with test data, predict will use best model
confusionMatrix(predict(train_knn_cv, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]


#different cross validation, just for playing
set.seed(2008)
control <- trainControl(method = "cv", number = 100, p = .8)
train_knn_my_cv <- train(y ~ ., method = "knn",
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_my_cv, highlight = TRUE)

##compare accuracy with test data, predict will use best model
confusionMatrix(predict(train_knn_my_cv, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

#different measures for accuracy 
names(train_knn_my_cv$results)

#plot prediction
plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])
plot_cond_prob(predict(train_knn_cv, mnist_27$true_p, type = "prob")[,2])
plot_cond_prob(predict(train_knn_my_cv, mnist_27$true_p, type = "prob")[,2])

# use gam package

#learn about the parameters
modelLookup("gamLoess")

#try out degree 1 only with different span
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1


## exercise
#Q1
library(tidyverse)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results


#Q2
pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal=TRUE)$p.value
}
ind <- which(pvals <= 0.01)
length(ind)

#Q3
x_subset <- x[ ,ind]

set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results

#Q4
set.seed(1)
fit <- train(x_subset, y, method = "knn",
                   tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)


#Q5
#devtools::install_bioc("genefilter") 
#install.packages("genefilter") 
#library(genefilter)

# Trees and randow forrests
#Load tidyverse
library(tidyverse)

# load package for decision tree
library(rpart)

# load the dslabs package
library(dslabs)

# fit a decision tree using the polls_2008 dataset, 
# which contains only one predictor (day)
# and the outcome (margin)
fit <- rpart(margin ~ ., data = polls_2008)

# display the decision tree
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

# examine the fit from the decision tree model
polls_2008 %>%  
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# fit a decision tree on the mnist data using cross validation
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
# and plot it
plot(train_rpart)

# compute accuracy
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# view the final decision tree
plot(train_rpart$finalModel, margin = 0.1) # plot tree structure
text(train_rpart$finalModel) # add text labels

# load library for random forest
library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# use cross validation to choose parameter
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]


##Comprehension Check: Trees and Random Forests
#Q1
library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat) 


#Q2
plot(fit) # plot tree structure
text(fit) # add text labels

#Q3
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

#Q4
library(randomForest)
fit <- randomForest(y ~ x, data = dat) 

  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

#Q5
plot(fit)  

#Q6
library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize=50, maxnodes=25) 
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
  
##Comprehension Caret Package
#Q1
library(dslabs)
library(caret)
library(rpart)
data("tissue_gene_expression")

set.seed(1991)
fit  <- train(tissue_gene_expression$x,tissue_gene_expression$y,
      method = "rpart",
      tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))
ggplot(fit)

#Q2
library(dslabs)
library(caret)
library(rpart)
data("tissue_gene_expression")

set.seed(1991)
fit  <- train(tissue_gene_expression$x,tissue_gene_expression$y,
              method = "rpart",
              tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
              control = rpart.control(minsplit = 0))
ggplot(fit)
fit

#Q3
plot(fit$finalModel)
text(fit$finalModel)


#Q4
library(dslabs)
library(caret)
library(rpart)
library(doParallel)


data("tissue_gene_expression")

registerDoParallel(cores=detectCores())
set.seed(1991)
fit  <- train(tissue_gene_expression$x,tissue_gene_expression$y,
              method = "rf",
              tuneGrid = data.frame(mtry = seq(50, 200, 25)),
              nodesize=1)
ggplot(fit)


### titanic Exercise
ibrary(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

str(titanic_clean)

#Q1
set.seed(42)
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
test_set <- titanic_clean[test_index, ]
train_set <- titanic_clean[-test_index, ]
nrow(train_set)
nrow(test_set)

train_set |> filter(Survived=="1") |> nrow()/nrow(train_set)
#or
train_set |> summarize(mean(as.numeric(Survived)-1))
#or
mean(train_set$Survived=="1")


#Q2
set.seed(3)
Survived_hat <-sample(c(0,1),nrow(test_set),replace=TRUE)
str(Survived_hat)
Survived_hat <- factor(Survived_hat, levels = levels(test_set$Survived))
confusionMatrix(Survived_hat, test_set$Survived)$overall[["Accuracy"]]
#or
mean(Survived_hat == test_set$Survived)


#Q3
train_set |> group_by(Sex) |> summarize(mean(as.numeric(Survived)-1))

#Q4
Survived_hat <- test_set$Sex=="female"
Survived_hat <- factor(as.numeric(Survived_hat), levels = levels(test_set$Survived))
str(Survived_hat)
cm_sex <- confusionMatrix(Survived_hat, test_set$Survived)
cm_sex$overall[["Accuracy"]]

#Q4a
train_set |> group_by(Pclass) |> summarize(mean(as.numeric(Survived)-1))

#Q4b
Survived_hat <- test_set$Pclass==1
Survived_hat <- factor(as.numeric(Survived_hat), levels = levels(test_set$Survived))
str(Survived_hat)
cm_class <- confusionMatrix(Survived_hat, test_set$Survived)
cm_class$overall[["Accuracy"]]

#Q4c
train_set |> group_by(Sex, Pclass) |> summarize(mean(as.numeric(Survived)-1))

#Q4d
Survived_hat <- test_set$Pclass %in% c(1,2) & test_set$Sex=="female"
Survived_hat <- factor(as.numeric(Survived_hat), levels = levels(test_set$Survived))
str(Survived_hat)
cm_sex_class <- confusionMatrix(Survived_hat, test_set$Survived)
cm_sex_class$overall[["Accuracy"]]

#Q5a
cm_sex
cm_sex$byClass["Sensitivity"]
cm_class$byClass["Sensitivity"]
cm_sex_class$byClass["Sensitivity"]
cm_sex$byClass["Specificity"]
cm_class$byClass["Specificity"]
cm_sex_class$byClass["Specificity"] #Balanced Accuracy
cm_sex$byClass["Balanced Accuracy"]
cm_class$byClass["Balanced Accuracy"]
cm_sex_class$byClass["Balanced Accuracy"]

#Q5b
Survived_hat_sex <- test_set$Sex=="female"
Survived_hat_sex <- factor(as.numeric(Survived_hat_sex), levels = levels(test_set$Survived))
F_meas(data = Survived_hat_sex, reference = test_set$Survived)

Survived_hat_class <- test_set$Pclass==1
Survived_hat_class <- factor(as.numeric(Survived_hat_class), levels = levels(test_set$Survived))
F_meas(data = Survived_hat_class, reference = test_set$Survived)

Survived_hat_sex_class <- test_set$Pclass %in% c(1,2) & test_set$Sex=="female"
Survived_hat_sex_class <- factor(as.numeric(Survived_hat_sex_class), levels = levels(test_set$Survived))
F_meas(data = Survived_hat_sex_class, reference = test_set$Survived)


#check
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1, # count family members
         LastName= str_replace( str_extract(Name, "[a-zA-Z]*, "),  ",", ""),
         Ethnicity=predict_ethnicity( lastnames = LastName, method="lastname"),
         Race=Ethnicity$race) %>%    
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked, LastName, Race)

titanic_clean |> group_by(Race) |> summarize(mean(as.numeric(Survived)-1))
titanic_clean |> group_by(Race) |> summarize(mean(as.numeric(levels(Survived)[Survived])))

#Q7
set.seed(1)

#learn about the parameters
modelLookup("gamLoess")

#try out degree 1 only with different span
grid <- expand.grid(span = seq(0.1, 0.9, len = 19), degree = 1)

train_loess <- train(Survived ~ Fare, 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = train_set)
plot(train_loess)
confusionMatrix(data = predict(train_loess, test_set), 
                reference = test_set$Survived)$overall["Accuracy"]

#but here they want just default tining
train_loess <- train(Survived ~ Fare, 
                     method = "gamLoess",
                     data = train_set)
confusionMatrix(data = predict(train_loess, test_set), 
                reference = test_set$Survived)$overall["Accuracy"]
#Q8
set.seed(1)

#learn about the parameters
modelLookup("glm")
train_glm <- train(Survived ~ Age, 
                     method = "glm",
                     data = train_set)
confusionMatrix(data = predict(train_glm, test_set), 
                reference = test_set$Survived)$overall["Accuracy"]


set.seed(1)
train_glm <- train(Survived ~ Sex+Pclass+Fare+Age, 
                   method = "glm",
                   data = train_set)
confusionMatrix(data = predict(train_glm, test_set), 
                reference = test_set$Survived)$overall["Accuracy"]

str(train_set)
set.seed(1)
train_glm <- train(Survived ~ Sex+Pclass+Fare+Age+SibSp+Parch+FamilySize+Embarked, 
                   method = "glm",
                   data = train_set)
confusionMatrix(data = predict(train_glm, test_set), 
                reference = test_set$Survived)$overall["Accuracy"]


#Q9a
set.seed(6)
modelLookup("knn")
#try out different k
grid <- expand.grid(k = seq(3, 51, 2) )

train_knn <- train(Survived ~ ., 
                   method = "knn",
                   tuneGrid=grid,
                   data = train_set)
plot(train_knn)
train_knn$bestTune
confusionMatrix(data = predict(train_knn, test_set), 
                reference = test_set$Survived)$overall["Accuracy"]

#Q10
set.seed(8)
#try out different k
grid <- expand.grid(k = seq(3, 51, 2) )
control <- trainControl(method = "cv", number = 10, p = .1)


train_knn <- train(Survived ~ ., 
                   method = "knn",
                   tuneGrid=grid,
                   data = train_set,
                   trControl = control)
plot(train_knn)
train_knn$bestTune
confusionMatrix(data = predict(train_knn, test_set), 
                reference = test_set$Survived)$overall["Accuracy"]


#Q11
set.seed(10)
modelLookup("rpart")
#try out different cp (complexity parameter)
grid <- expand.grid(cp = seq(0, 0.05, 0.002) )

train_rpart <- train(Survived ~ ., 
                   method = "rpart",
                   tuneGrid=grid,
                   data = train_set)
plot(train_rpart)
train_rpart$bestTune
confusionMatrix(data = predict(train_rpart, test_set), 
                reference = test_set$Survived)$overall["Accuracy"]

#Q11b
plot(train_rpart$finalModel)
text(train_rpart$finalModel)


#Q11c
predict(train_rpart, data.frame(Sex="male",Pclass=1,Age=28,Fare=7.92,SibSp=0,Parch=0,FamilySize=1,Embarked="S"))
predict(train_rpart, data.frame(Sex="female",Pclass=2,Age=28,Fare=7.92,SibSp=0,Parch=0,FamilySize=1,Embarked="S"))
predict(train_rpart, data.frame(Sex="female",Pclass=3,Age=28,Fare=8,SibSp=0,Parch=0,FamilySize=1,Embarked="S"))
predict(train_rpart, data.frame(Sex="male",Pclass=3,Age=5,Fare=8,SibSp=4,Parch=0,FamilySize=1,Embarked="S"))
predict(train_rpart, data.frame(Sex="female",Pclass=3,Age=28,Fare=25,SibSp=0,Parch=0,FamilySize=1,Embarked="S"))
predict(train_rpart, data.frame(Sex="male",Pclass=1,Age=17,Fare=25,SibSp=2,Parch=0,FamilySize=1,Embarked="S"))

#Q12
set.seed(14)
modelLookup("rf")
#try out different mtry (complexity parameter)
grid <- expand.grid(mtry = seq(1:7) )

train_rf <- train(Survived ~ ., 
                     method = "rf",
                     tuneGrid=grid,
                     data = train_set,
                  ntree=100)
ggplot(train_rf)
plot(train_rf)
train_rf$bestTune
confusionMatrix(data = predict(train_rf, test_set), 
                reference = test_set$Survived)$overall["Accuracy"]


## Preprocessing

library(dslabs)
mnist <- read_mnist()

names(mnist)
dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)

# sample 10k rows from training set, 1k rows from test set
set.seed(1990)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

#investigate which predictors have small variation
library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

#eliminate predictors with small SD
library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

#KNN

#train needs column names
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

#this takes a some  time, use all cores
registerDoParallel(cores=detectCores())
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

# a sample is much faster
n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
#now train with k=3
fit_knn <- knn3(x[ ,col_index], y,  k = 3)

#predict
y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

cm$byClass[,1:2]

#RandomForest

library(randomForest)
control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))
#train with 5 fold cross validation and a 5000 sample
train_rf <-  train(x[, col_index], y,
                   method = "rf",
                   nTree = 150,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
# train model with full sample mtry=5, not parallel
fit_rf <- randomForest(x[, col_index], y,
                       minNode = train_rf$bestTune$mtry)

y_hat_rf <- predict(fit_rf, x_test[ ,col_index])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]


#Variable importance

imp <- importance(fit_rf)
imp

mat <- rep(0, ncol(x))
mat[col_index] <- imp
image(matrix(mat, 28, 28))


p_max <- predict(fit_rf, x_test[,col_index], type = "prob") 
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)

ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]

#install.packages("rafalib")
library(rafalib)
rafalib::mypar(1,4)
for(i in ind[1:4]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}


#Ensembling

#randowm forest
p_rf <- predict(fit_rf, x_test[,col_index], type = "prob")
# norm to 1
p_rf <- p_rf / rowSums(p_rf)

#predict knn
p_knn <- predict(fit_knn, x_test[,col_index])

#take the average
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)$overall["Accuracy"]


##exercises

#Q1
models <- c("glm", "lda", "naive_bayes", "knn", "gamLoess", "qda", "rf")

library(caret)
library(dslabs)
library(tidyverse)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

#Q2
predicts <- sapply(fits, function(fit) {
  predict(fit, newdata=mnist_27$test)
})
str(predicts)
dim(predicts)

#Q3
#manually calc all models
mean(predicts[,]==mnist_27$test$y)

#each model separately
accu <- rep(0, length(models))
for (i in 1:length(models)){
  accu[i] <- mean(predicts[,i] == mnist_27$test$y)
}
mean(accu)

#each model separately using confusionMatrix
for (i in 1:length(models)){
  accu[i] <- confusionMatrix(factor(predicts[,i]), mnist_27$test$y)$overall["Accuracy"]
}
mean(accu)

#more elegant
accu <- colMeans(predicts == mnist_27$test$y)
accu
mean(accu)

#Q4
ensemble  <- rep(0, length(mnist_27$test$y))

for (i in 1:length(ensemble)){
  ensemble[i] <- if (sum(predicts[i,]=="7")>3) "7" else "2"
}

confusionMatrix(factor(ensemble), mnist_27$test$y)$overall["Accuracy"]

#or
votes <- rowMeans(predicts == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

#Q5
ind <- accu > mean(ensemble == mnist_27$test$y)
sum(ind)
models[ind]


#Q6
accu2 <- sapply(fits, function(fit) {
  min(fit$results$Accuracy)
})
mean(accu2)

#Q7
ind <- accu2 > .8
sum(ind)
ensemble  <- rep(0, length(mnist_27$test$y))

for (i in 1:length(ensemble)){
  ensemble[i] <- if (sum(predicts[i,ind]=="7")> sum(ind)/2) "7" else "2"
}

confusionMatrix(factor(ensemble), mnist_27$test$y)$overall["Accuracy"]

#or

votes <- rowMeans(predicts[,ind] == "7"  )
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)



## Netflix challenge

library(dslabs)
library(tidyverse)
data("movielens")

str(movielens)
movielens %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

movielens %>%  
  ggplot(aes(userId)) + 
  geom_histogram(binwidth=100)

mov <- movielens %>%
  group_by(userId) %>%
  summarize(n=n()) 

mov %>%  
  filter(n >50) %>%
  ggplot(aes(n)) + 
  geom_histogram(binwidth=50) 


# create train set and test set t, use semi_join() function to ensure that we don’t include users and movies in the test set that do not appear in the training set. The code used for the purpose is as follows:

library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Finally we use pivot_wider to make a matrix with users represented by rows and movies by the columns
y <- select(train_set, movieId, userId, rating) |>
  pivot_wider(names_from = movieId, values_from = rating) 
rnames <- y$userId
y <- as.matrix(y[,-1])
rownames(y) <- rnames

#along with a table to map movie ids to titles:
movie_map <- train_set |> select(movieId, title) |> distinct(movieId, .keep_all = TRUE)

# build first model
#RSME function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu_hat <- mean(train_set$rating)
mu_hat
mu<- mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

#The following code confirms that any number other than  would result into a higher RMSE .

predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)

mu_other <- seq(1,5,0.01)
RMSEs <- sapply(mu_other, function(mu_hat){
  predictions <- rep(mu_hat, nrow(test_set))
  RMSE(test_set$rating, predictions)
})
mu_other[which.min(RMSEs)]
RMSEs[which.min(RMSEs)]

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)

#first model, lm takes a lot of time (well...)
#fit <- lm(rating ~ as.factor(movieId), data = movielens)

b_i <- colMeans(y - mu, na.rm = TRUE)
qplot(b_i, bins = 10, color = I("black"))

#Let’s see how much our prediction improves 
fit_movies <- data.frame(movieId = as.integer(colnames(y)), 
                         mu = mu, b_i = b_i)
movie_rmse <- left_join(test_set, fit_movies, by = "movieId") |> 
  mutate(pred = mu + b_i) |> 
  summarize(rmse = RMSE(rating, pred)) |> 
  pull(rmse)

rmse_results <-  union(rmse_results, tibble(method = "Movie effect", RMSE = movie_rmse))
rmse_results

# calculate movie_avgs for later
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))


#Let’s compute the average rating for user  
b_u <- rowMeans(y, na.rm = TRUE)
qplot(b_u, bins = 30, color = I("black"))


#To fit this model, we could again use lm like this:
#lm(rating ~ as.factor(movieId) + as.factor(userId))

#but, instead, we will compute an approximation by computing  
b_u <- rowMeans(sweep(y - mu, 2, b_i), na.rm = TRUE)

#We can now construct predictors and see how much the RMSE improves:
fit_users <- data.frame(userId = as.integer(rownames(y)), b_u = b_u)

#test it
user_rmse <- left_join(test_set, fit_movies, by = "movieId") |> 
  left_join(fit_users, by = "userId") |> 
  mutate(pred = mu + b_i + b_u) |> 
  summarize(rmse = RMSE(rating, pred)) |> 
  pull(rmse)

rmse_results <-  union(rmse_results, tibble(method = "Movie # user effect", RMSE = user_rmse))
rmse_results


#Exercise
library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

#Q1

movielens %>%
  group_by(movieId,year) %>%
  summarize(n=n(), year=first(year)) %>%
  ggplot(aes(factor(year), n)) +
  geom_boxplot() +
  scale_y_continuous(trans = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
movielens %>%
  group_by(movieId,year) %>%
  summarize(n=n(), year=first(year)) %>%
  group_by(year) %>%
  summarize(m=median(n)) %>%
  arrange(desc(m))

movielens %>%
  group_by(movieId,year) %>%
  summarize(n=n()) %>%
  group_by(year) %>%
  summarize(m=median(n)) %>%
  arrange(desc(m))


#Q2
movielens %>%
  group_by(movieId,title, year) %>%
  summarize(n=n(), year=first(year), rating=mean(rating)) %>%
  filter(year >= 1993) %>%
  group_by(movieId,title) %>%
  summarize(n=sum(n)/(2018-year), m=mean(rating)) %>%
  arrange(desc(n))



# how many years have the movies?
movielens |>
  group_by(movieId) |>
  summarize(n_movies = n_distinct(year)) |>
  arrange(desc(n_movies))

#Q3
movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId,title, year) %>%
  summarize(n=n(), year=first(year), rating=mean(rating)) %>%
  group_by(movieId,title) %>%
  summarize(n=sum(n)/(2018-year), m=mean(rating)) %>%
  ggplot(aes(n,m))+
  geom_point()+
  geom_smooth(method="lm")

#Q5
movielens <- mutate(movielens, date = as_datetime(timestamp))

#Q6
movielens %>%
  mutate(week=round_date(date,"week")) %>%
  group_by(week) %>%
  summarize(rating=mean(rating)) %>%
  ggplot(aes(week,rating))+
  geom_point()+
  geom_smooth(method="lm")

#Q8

movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Regularization
library(dslabs)

#where is the biggest error?
n <-  colSums(!is.na(y))
fit_movies$n <- n
best <- fit_movies |> left_join(movie_map, by = "movieId") |> 
  mutate(average_rating = mu + b_i) |>
  filter(average_rating == 5 & n>1) 
test_set |> 
  group_by(movieId) |>
  summarize(test_set_average_rating = mean(rating)) |>
  right_join(best, by = "movieId") |>
  select(title, average_rating, n, test_set_average_rating)


# use cross-validation to find lamda
lambdas <- seq(0, 10, 0.1)

sums <- colSums(y - mu, na.rm = TRUE)
rmses <- sapply(lambdas, function(lambda){
  b_i <-  sums / (n + lambda)
  fit_movies$b_i <- b_i
  left_join(test_set, fit_movies, by = "movieId") |> mutate(pred = mu + b_i) |> 
    summarize(rmse = RMSE(rating, pred)) |>
    pull(rmse)
})

qplot(lambdas, rmses, geom = "line")
lambda <- lambdas[which.min(rmses)]
print(lambda)

# calculate regulized estimates
fit_movies$b_i_reg <- colSums(y - mu, na.rm = TRUE) / (n + lambda)

#test it
fit_users$b_u <- rowMeans(sweep(y - mu, 2, b_i), na.rm = TRUE)
rmse_reg <- left_join(test_set, fit_movies, by = "movieId") |> 
  left_join(fit_users, by = "userId") |> 
  mutate(pred = mu + b_i_reg + b_u) |> 
  summarize(rmse = RMSE(rating, pred)) |> 
  pull(rmse)

rmse_results <-  union(rmse_results, tibble(method = "Regularized Movie + User Effect Model", RMSE = rmse_reg))
rmse_results


##Exercise

options(digits=7)
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

# assign a true quality for each school 
set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

# We can see the top 10 schools using this code: 
schools %>% top_n(10, quality) %>% arrange(desc(quality))
  
#This code will simulate the test scores:
set.seed(1)
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#alternative calculation
score2 <- sapply(scores, function(x)  mean(x))
schools <- schools %>% mutate(score2 = score2) 


#Q1
schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)

#Q2
median(schools$size)
schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score) %>%
  summarize(m=median(size))

#Q3
schools %>% top_n(-10, score) %>% arrange(score) %>% select(id, size, score) %>%
  summarize(m=median(size))

#Q4

schools |> ggplot(aes(x=size,y=score,color=(rank<=10))) +
  geom_point()

#or

schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)

#Q5
overall <- mean(sapply(scores, mean))
alpha <-25
schools <- schools %>% 
  mutate(n =  sapply(scores, sum)/ sapply(scores, mean),
         score2 = overall + (sapply(scores, sum) - n*overall) / (n+alpha))
schools %>% head(5)

schools %>% top_n(10, score2) %>% arrange(desc(score2)) %>% select(id, size, score2)

#oder
alpha <- 25

score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#Q6
#RSME function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
overall <- mean(sapply(scores, mean))


# use cross-validation to find lamda
alphas <- seq(10, 250, 1)

rmses <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
  schools %>% mutate(score_reg = score_reg) %>%
    summarize(rmse = RMSE(quality, score_reg)) %>%
    pull(rmse)
})

qplot(alphas, rmses, geom = "line")
alpha <- alphas[which.min(rmses)]
print(alpha)

#Q7

score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))


#Q8
# use cross-validation to find lamda WRONG without centering!!!
alphas <- seq(10, 250, 1)

rmses <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x)   sum(x)/(length(x)+alpha))
  schools %>% mutate(score_reg = score_reg) %>%
    summarize(rmse = RMSE(quality, score_reg)) %>%
    pull(rmse)
})

qplot(alphas, rmses, geom = "line")
alpha <- alphas[which.min(rmses)]
print(alpha)



## Matrix Factorization

#create a subset of the data (many ratings and Scent of a Woman)
train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>%
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>% 
  dplyr::select(userId, movieId, rating) %>%
  pivot_wider(names_from = "movieId", values_from = "rating") %>%
  as.matrix()

rownames(y)<- y[,1]
y <- y[,-1]

movie_titles <- movielens %>% 
  dplyr::select(movieId, title) %>%
  distinct()

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)]) 

#convert to residuals 
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))


#PCA
y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)

dim(pca$rotation)

dim(pca$x)

plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

library(ggrepel)
pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))

pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)

pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)


##Exercise
set.seed(1987)
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))


#Q1
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}
my_image(y)

#subsets
my_image(y[1:2,6:11])
y[1:2,6:11]


#Q2
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)


#Q3
s <- svd(y)
names(s)

#test Y=UDV V transformed)
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

ss_y <- colSums(y^2)
yv <- y %*% s$v
ss_yv <- colSums(yv^2)
sum(ss_y)
sum(ss_yv)

#oder
ss_y <- apply(y^2, 2, sum)
ss_yv <- apply((y%*%s$v)^2, 2, sum)
sum(ss_y)
sum(ss_yv)


#Q4

plot(ss_y)
plot(ss_yv)


#Q5
plot(sqrt(ss_yv),s$d)

#or
data.frame(x = sqrt(ss_yv), y = s$d) %>%
  ggplot(aes(x,y)) +
  geom_point()

#Q6

first <- yv[,1:3]
sum(first^2)/sum(y^2)

#or

sum(s$d[1:3]^2) / sum(s$d^2)

#Q7
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))


#Q8
col1 <- s$u[,1] * s$d[1]
avgs <- rowMeans(y)
plot(avgs,col1)


#Q9
my_image(s$v)


#Q10
plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))

with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
my_image(y)

#Q11
resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))

with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))
my_image(resid)

#Q12
sum(s$d[1:2]^2)/sum(s$d^2) * 100

resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))

with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))
my_image(resid)

#Q13
sum(s$d[1:3]^2)/sum(s$d^2) * 100

resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(y, zlim = range(y))
my_image(y_hat, zlim = range(y))
my_image(y - y_hat, zlim = range(y))

##Breast Cancer Project
options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)


#Q1

str(brca)
length(brca$y)
mean(brca$y=="M")
which.max(colMeans(brca$x))
which.min(colSds(brca$x))

#Q2

cms <- apply(brca$x, 2, mean)
x <- sweep(brca$x,2,cms, FUN="-")
sds <- apply(x, 2, sd)
x <- sweep(x,2,sds, FUN="/")

#oder
x <- sweep(brca$x, 2, colMeans(brca$x))
x <- sweep(x, 2, colSds(brca$x), FUN = "/")
x_scaled <- x

sd(x[,1])
median(x[,1])
mean(x[,1])

#Q3
pca <- prcomp(x)

dim(pca$rotation)

dim(pca$x)

plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)
var_explained[1]
sum(var_explained < .9)
var_explained[6]
var_explained[7]

#oder
summary(pca)


#Q4

pca.pred <- predict(pca)
pcs <- data.frame(PC1= pca.pred[, 1], 
                  PC2= pca.pred[, 2], 
                  type=brca$y)
pcs %>%  ggplot(aes(x=PC1, y=PC2,color=type)) + geom_point() +
  scale_color_manual(values = c("black", "red"))

#or

data.frame(pca$x[,1:2], type = brca$y) %>%
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point()

identical(pca.pred,pca$x)

#Q5
  
data.frame(pca$x[,1:10], type = brca$y) %>%
  pivot_longer(cols=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10")) %>%
  mutate(name = paste(name,type)) %>%
  ggplot(aes(name,value,fill=type)) +
  geom_boxplot()

#oder

data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()

#Q6
set.seed(1) 
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)

test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]


mean(train_y=="B")
mean(test_y=="B")

#Q7
registerDoParallel(cores=detectCores())

set.seed(1) 


train_glm <- train(x=train_x,
                     y=train_y,
                     method = "glm")
confusionMatrix(predict(train_glm, test_x, type = "raw"), test_y)$overall[["Accuracy"]]



#Q8

set.seed(5) 

train_loess <- train(x=train_x,
                     y=train_y,
                     method = "gamLoess")
confusionMatrix(predict(train_loess, test_x, type = "raw"), test_y)$overall[["Accuracy"]]

#Q9

set.seed(7) 

train_knn <- train(x=train_x,
                     y=train_y,
                     method = "knn",
                     tuneGrid = data.frame(k = seq(3, 21, 2)))
train_knn                
confusionMatrix(predict(train_knn, test_x, type = "raw"), test_y)$overall[["Accuracy"]]

#Q10
set.seed(9) 

train_rf <- train(x=train_x,
                   y=train_y,
                   method = "rf",
                   tuneGrid = expand.grid(mtry = c(3, 5, 7, 9)),
                   importance = TRUE)
train_rf                
confusionMatrix(predict(train_rf, test_x, type = "raw"), test_y)$overall[["Accuracy"]]
train_rf$bestTune
varImp(train_rf)

#Q11

models <- c("glm", "gamLoess", "knn", "rf")

p_glm <- predict(train_glm, test_x, type = "raw")
p_loess <- predict(train_loess, test_x, type = "raw")
p_knn <- predict(train_knn, test_x, type = "raw")
p_rf <- predict(train_rf, test_x, type = "raw")

predicts <- (p_glm == "B") + (p_loess == "B") + (p_knn == "B") + (p_rf == "B")

y_hat <- ifelse(predicts >= 2, "B", "M")
mean(y_hat == test_y)


