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


library(mosaic) # Diesen Befehl bei jeder Session am Anfang ausführen
library(readr)
library(gplots)
library(readxl)


## Section 1
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

#home runs and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#stolen bases and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#base on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)


#base at bats and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#win rate and fielding errors
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(win_rate = W / G, E_per_game = E / G) %>%
  ggplot(aes(win_rate, E_per_game)) + 
  geom_point(alpha = 0.5)

#triples and doubles
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B / G, X2B_per_game = X2B / G) %>%
  ggplot(aes(X3B_per_game, X2B_per_game)) + 
  geom_point(alpha = 0.5)


##correlation
# create the dataset
library(tidyverse)
library(HistData)
data("GaltonFamilies")
GaltonFamilies %>% head()

set.seed(1983)

#select one sone per family randomly
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>% head()


# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

# father-son correlation
galton_heights %>% summarize(cor(father, son))
#oder
cor(father~son,data=galton_heights)


# compute sample correlation
my_sample <- slice_sample(galton_heights, n = 25, replace = TRUE)

R <- my_sample %>% summarize(cor(father, son))
R

# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
  slice_sample(galton_heights, n = N, replace = TRUE) %>% 
    summarize(r=cor(father, son)) %>% .$r
})
data.frame(R) %>% ggplot(aes(R)) + geom_histogram(binwidth = 0.05, color = "black")

# expected value is the population correlation
mean(R)
# standard error is high relative to its size
sd(R)

# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))


#exercise
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

#home runs and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(R_per_game = R / G, AB_per_game = AB / G) %>%
  ggplot(aes(R_per_game, AB_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(R_per_game = R / G, AB_per_game = AB / G) %>%
  summarize(cor(R_per_game, AB_per_game)) 

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(win_rate = W / G, E_per_game = E / G) %>%
  summarize(cor(win_rate, E_per_game)) 

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B / G, X2B_per_game = X2B / G) %>%
  summarize(cor(X3B_per_game, X2B_per_game)) 

Teams %>% filter(yearID %in% 1961:2001 ) %>% 
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>% 
  summarize(cor(AB_per_game, R_per_game))


##introducing regression
# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)

# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg

# stratify fathers' heights to make a boxplot of son heights
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# add regression line to standardized data
r <- galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

galton_heights %>% 
  mutate(father = scale(father), son = scale(son)) %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son = mean(son)) %>%
  ggplot(aes(father, son)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = r)

# add regression line to original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <-  r * s_y / s_x
b <- mu_y - m*mu_x

galton_heights %>% 
  ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m )

# plot in standard units and see that intercept is 0 and slope is rho
galton_heights %>% 
  ggplot(aes(scale(father), scale(son))) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r)

#qq plots of all possible father heights 
galton_heights %>%
  mutate(z_father = round((father - mean(father))/sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample=son)) +
  facet_wrap(~z_father)


# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <-  r * s_y / s_x
b <- mu_y - m*mu_x

galton_heights %>% 
  ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m )

# compute a regression line to predict the father's height from the son's height
m <-  r * s_x / s_y
b <- mu_x - m*mu_y
galton_heights %>% 
  ggplot(aes(son, father)) + 
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m )

# standardized compute a regression line to predict the son's height from the father's height
mu_x <- mean(scale(galton_heights$father))
mu_y <- mean(scale(galton_heights$son))
s_x <- sd(scale(galton_heights$father))
s_y <- sd(scale(galton_heights$son))
r <- cor(scale(galton_heights$father), scale(galton_heights$son))
m <-  r * s_y / s_x
b <- mu_y - m*mu_x

#exercise
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mu_x <- mean(female_heights$mother)
s_x <- sd(female_heights$mother)
mu_y <- mean(female_heights$daughter)
s_y <- sd(female_heights$daughter)
r <- female_heights %>% summarize(c=cor(mother,daughter)) %>% pull(c)
m <-  r * s_y / s_x
b <- mu_y - m*mu_x
m
b
r*r*100
m*60+b


## confound
# find regression line for predicting runs from BBs (not shown in video)
library(tidyverse)
library(Lahman)
get_slope <- function(x, y) cor(x, y) * sd(y) / sd(x)

bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  summarize(slope = get_slope(BB_per_game, R_per_game))

bb_slope 

# compute regression line for predicting runs from singles (not shown in video)
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  summarize(slope = get_slope(Singles_per_game, R_per_game))

singles_slope 

# calculate correlation between HR, BB, and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))


##Stratification and Multivariate Regression
# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

# calculate slope of regression line after stratifying by HR
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 

#exercise
lm(son ~ father, data = galton_heights)

galton_heights <- galton_heights %>%
  mutate(father_centered=father - mean(father))

lm(son ~ father_centered, data = galton_heights)

# compute RSS for any pair of beta0 and beta1 in Galton's data
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# plot RSS as a function of beta1 when beta0=25
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

# fit regression line to predict son's height from father's height
fit <- lm(son ~ father, data = galton_heights)
fit

# summary statistics
summary(fit)


# Monte Carlo simulation of linear model
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

# Plot the distribution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

# summary statistics with std error
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>%
  .$coef

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

#strong correlation on non-standardized data
lse %>% summarize(cor(beta_0, beta_1))

#standardize
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})
# much smaller correlation
cor(lse[1,], lse[2,])


# plot predictions and confidence intervals
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)

# plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line()


#exercise
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)


# calculate linear model of R by HR and BB, and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(R = R/G, BB = BB/G, HR = HR/G) %>%  
  lm( R ~ BB + HR, data=. )


#monte carlo
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 


#plots
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth()

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", linewidth = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", linewidth = 1) + 
  geom_point(data = galton_heights, aes(x = father, y = son))


#female heights
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

summary(lm(mother~daughter,data=female_heights))

fit <- lm(mother~daughter,data=female_heights)
p<- predict(fit)
p[1]
p
female_heights$mother[1]

#baseball again
library(Lahman)

#year 2002
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

#years 1999-2001
bat_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

players <- bat_01 %>% group_by(playerID) %>%
 summarize(mean_singles = mean(singles),
 mean_bb = mean(bb))
players %>% head
sum(players$mean_singles>0.2)
sum(players$mean_bb>0.2)

pc <- bat_02 %>% inner_join(players,by="playerID")
pc %>% head
cor(singles~mean_singles, data=pc)
cor(bb~mean_bb, data=pc)

pc %>% ggplot(aes(x=singles,y=mean_singles))+
    geom_point(alpha=.5)
pc %>% ggplot(aes(x=bb,y=mean_bb))+
  geom_point(alpha=.5)

lm(singles~mean_singles, data=pc)
lm(bb~mean_bb, data=pc)


## linear regression with tidyverse

# stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

# calculate slope of regression lines to predict runs by BB in different HR strata
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

# use lm to get estimated slopes - lm does not work with grouped tibbles
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef

# include the lm inside a summarize and it will work
dat %>%  
  group_by(HR) %>%
  summarize(slope = lm(R ~ BB)$coef[2])

# tidy function from broom returns estimates in and information in a data frame
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)

# add confidence intervals
tidy(fit, conf.int = TRUE)

# combine with group_by and summarize to get the table we want
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE))

# it's a data frame so we can filter and select the rows and columns we want
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

# visualize the table with ggplot
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

# EXTRA CODE TO DEMONSTRATE THE USE OF across()
# Compare the output of the 3 options below:
dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) 
# Incorrect, will provide identical estimates for all groups


  
  dat %>%  
    group_by(HR) %>%
    summarize(tidy(lm(R ~ BB, data = across(c(R,BB))), conf.int = TRUE))
  # Correct option 1, provides distinct estimates for all groups
  
  dat %>%  
  group_by(HR) %>%
  reframe(tidy(lm(R ~ BB, data = across(c(R,BB))), conf.int = TRUE))
# Correct option 1, provides distinct estimates for all groups



dat %>%  
  group_by(HR) %>%
  reframe(tidy(lm(R ~ BB), conf.int = TRUE)) 
# Correct option 2, provides distinct estimates for all groups


#exercise

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)


get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

dat %>% 
  group_by(HR) %>% 
  summarize(slope = get_slope(across()))

dat %>% 
  group_by(HR) %>% 
  summarize(get_slope(across()))

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R)
dat %>% head

dat %>% 
  group_by(lgID) %>% 
  reframe(tidy(lm(R ~ HR, data = across(c(R,HR))), conf.int = T)) %>% 
  filter(term == "HR") 


#galton again
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton %>% group_by(pair) %>% 
  summarize(n=n())

galton %>% group_by(pair) %>% 
  summarize(cor(childHeight,parentHeight))

galton %>% group_by(pair) %>% 
reframe(tidy(lm(childHeight ~ parentHeight, data = across(c(childHeight, parentHeight))), conf.int = T)) %>% 
  filter(term == "parentHeight") %>%
  mutate(w=conf.high-conf.low)


## baseball again for multivaritae linear model
# linear regression with two variables
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

# predict number of runs for each team in 2002 and plot
# prediction requires number of BB, singles, etc in 2002!
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

# average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

# add 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
#find most played position
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
#join everything and filter defensive only with salary
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
# NOTE: In old versions of the Lahman library, the "People" dataset was called "Master"
# The following code may need to be modified if you have not recently updated the Lahman library.
players <- People %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

##sophomore slump
#The code to create a table with player ID, their names, and their most played position:
library(Lahman)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(People, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

#The code to create a table with only the ROY award winners and add their batting statistics:
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

#The code to keep only the rookie and sophomore seasons and remove players who did not play sophomore seasons:
  ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

#The code to use the spread function to have one column for the rookie and sophomore years batting averages:
  ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY

#The code to calculate the proportion of players who have a lower batting average their sophomore year:
mean(ROY$sophomore - ROY$rookie <= 0)

#The code to do the similar analysis on all players that played the 2013 and 2014 seasons and batted more than 130 times (minimum to win Rookie of the Year):
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years

#The code to see what happens to the worst performers of 2013:
arrange(two_years, `2013`)

#The code to see  the correlation for performance in two separate years:
qplot(`2013`, `2014`, data = two_years)


#measurement error
#The code to use dslabs function rfalling_object to generate simulations of dropping balls:
library(dslabs)
falling_object <- rfalling_object()
#The code to draw the trajectory of the ball:
falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")
#The code to use the lm() function to estimate the coefficients:
fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)

tidy(fit)

#The code to check if the estimated parabola fits the data:
augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue") 

#The code to see the summary statistic of the regression:
tidy(fit, conf.int = TRUE)
#fits with formular in physics!

#Add ribbon for confidence interval 
  augment(fit,interval="confidence") %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue") +
  geom_ribbon(aes(x=time, y=.fitted,ymin=.lower, ymax=.upper), alpha=0.2) 
  
##exercises
  
BB <- 0.371
singles <- 0.519
doubles <- 0.771
triples <- 1.24
HR <- 1.44
  
A <-2*BB + 4*singles + 1*doubles + 0*triples + 1*HR
A
B <-1*BB + 6*singles + 2*doubles + 1*triples + 0*HR
B  

library(Lahmann)
fit <- Teams %>% filter(yearID==1971) %>% 
  lm(R~BB+HR, data = .)
tidy(fit)

#or in one command
Teams %>% filter(yearID==1971) %>% 
  lm(R~BB+HR, data = .) %>%
  tidy(.) %>%
  filter(term=="BB")

Teams %>% filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(fit=lm(R~BB+HR, data = .)) %>%
  mutate(BB=fit$coefficients[2]) %>%
  select(yearID,BB) %>%
  ggplot(aes(x=yearID,y=BB))+
  geom_point() +
  geom_smooth(method = "lm")

#alternative  
Teams %>% filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R~BB+HR, data = .)) )%>%
  filter(term=="BB") %>%
  ggplot(aes(x=yearID,y=estimate))+
  geom_point() +
  geom_smooth(method = "lm")

#model the year impact
models <- Teams %>% filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R~BB+HR, data = .)) )%>%
  filter(term=="BB") %>%
  select(yearID,estimate)
models
summary(lm(estimate~yearID,data=models))


##more assessment
library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

Teams_small %>% mutate(R_pg = R/G) %>% lm(avg_attendance~R_pg, data=.)
Teams_small %>% mutate(HR_pg = HR/G) %>% lm(avg_attendance~HR_pg, data=.)
Teams_small %>% lm(avg_attendance~W, data=.)
Teams_small %>% lm(avg_attendance~yearID, data=.)
Teams_small %>% mutate(R_pg = R/G) %>% cor(R_pg~W, data=.)
Teams_small %>% mutate(HR_pg = HR/G) %>% cor(HR_pg~W, data=.)

stratas <- Teams_small %>% 
  mutate(strata=round(W/10)) %>%
  filter(strata %in% 5:10)
  
stratas %>% filter(strata==8) %>% nrow()

stratas %>% mutate(R_pg = R/G) %>%
  group_by(strata) %>%
  do(tidy(lm(avg_attendance~R_pg, data = .)) )%>%
  filter(term=="R_pg") %>%
  select(strata,estimate) %>%
  arrange(desc(estimate))
  
stratas %>% mutate(HR_pg = HR/G) %>%
  group_by(strata) %>%
  do(tidy(lm(avg_attendance~HR_pg, data = .)) )%>%
  filter(term=="HR_pg") %>%
  select(strata,estimate) %>%
  arrange(desc(estimate))


Teams_small %>% mutate(R_pg = R/G, HR_pg = HR/G) %>% lm(avg_attendance~R_pg+HR_pg+W+yearID, data=.)

fit <- Teams_small %>% mutate(R_pg = R/G, HR_pg = HR/G) %>% lm(avg_attendance~R_pg+HR_pg+W+yearID, data=.)

predict(fit,data.frame(R_pg=c(5), HR_pg=c(1.2), W=c(80), yearID=c(2002)))
predict(fit,data.frame(R_pg=c(5), HR_pg=c(1.2), W=c(80), yearID=c(1960)))

str(Teams_small)
Teams_small %>% group_by select(yearID) 
  
  Teams_small %>% filter(yearID == 2002) %>%
  mutate(R_pg = R/G, HR_pg = HR/G) 
  
Teams %>% filter(yearID==2002) %>%
  mutate(R_pg = R/G, HR_pg = HR/G, avg_attendance= attendance/G) %>%
  mutate( p=predict(fit,.)) %>%
  summarize(cor(p,avg_attendance))
   

##spurious correlation
# generate the Monte Carlo simulation
N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))

# calculate correlation between X,Y for each group
res <- sim_data %>% 
  group_by(group) %>% 
  summarize(r = cor(x, y)) %>% 
  arrange(desc(r))
res

#how many correlations are significant?
g <- 10000
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))
res <- sim_data %>% 
  group_by(group) %>% 
  reframe(tidy(cor.test(x, y)))%>% 
  arrange(desc(estimate))
mean(res$p.value<=0.05)



# plot points from the group with maximum correlation
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_smooth(method = "lm")


# histogram of correlation in Monte Carlo simulations
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")

# linear regression on group with maximum correlation
library(broom)
sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  reframe(tidy(lm(y ~ x)))


##outliers
# simulate independent X, Y and standardize all except entry 23
# note that you may get different values than those shown in the video depending on R version
set.seed(1985)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

# plot shows the outlier
qplot(x, y, alpha = 0.5)

# outlier makes it appear there is correlation
cor(x,y)
cor(x[-23], y[-23])

# use rank instead
qplot(rank(x), rank(y))
cor(rank(x), rank(y))

# Spearman correlation with cor function
cor(x, y, method = "spearman")

## reverse cause and effect
# cause and effect reversal using son heights to predict father heights
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>% reframe(tidy(lm(father ~ son)))

##confounder

# UC-Berkeley admission data
library(dslabs)
data(admissions)
admissions

# percent men and women accepted
admissions %>% group_by(gender) %>% 
  summarize(percentage = 
              round(sum(admitted*applicants)/sum(applicants),1))

# test whether gender and admission are independent
admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  summarize(tidy(chisq.test(.)))

# percent admissions by major
admissions %>% select(major, gender, admitted) %>%
  pivot_wider(names_from = gender, values_from = admitted) %>%
  mutate(women_minus_men = women - men)

# plot total percent admitted to major versus percent women applicants
admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants) / sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

# plot percent of applicants accepted by gender
admissions %>% 
  mutate(percent_admitted = admitted*applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

# plot admissions stratified by major
admissions %>% 
  ggplot(aes(major, admitted, col = gender, size = applicants)) +
  geom_point()

# average difference by major
admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))


#assessment
library(dslabs)
data("research_funding_rates")
research_funding_rates


datas <- research_funding_rates %>% mutate(non_men=applications_men-awards_men, non_women=applications_women-awards_women) %>%
  select(non_men, non_women, awards_men, awards_women) %>%
  summarize_all(sum) %>% 
  pivot_longer(names_to="tg", cols=everything())  %>%
  separate(tg, c("type", "gender"), sep = "_") %>%
  pivot_wider(names_from=type, values_from=value)
datas

datas %>% mutate(perc=awards/(awards+non)*100)

datas %>% select (non,awards) %>% chisq.test(.)

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  pivot_longer(-discipline) %>%
  separate(name, c("type", "gender")) %>%
  pivot_wider(names_from = type, values_from = value) %>%
  filter(gender != "total")
dat

dat %>% 
  ggplot(aes(x=reorder(discipline,success), success, col = gender, size = applications)) +
  geom_point()
