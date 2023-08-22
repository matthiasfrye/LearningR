library(tidyverse) #load also ggplot2
library(dslabs)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(titanic)
library(mosaic)


data(murders)
data(heights)

# make a table of category proportions
prop.table(table(heights$sex))

#probability function
my_data <- heights$height
a <- seq(min(my_data), max(my_data), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(my_data <= x) #logical vector my_data <= x, mean = relative Häufigkeit von TRUE
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)

#Male students
x <- heights %>% filter(sex=="Male") %>% pull(height)

#pnorm Students taller than 70.5
1 - pnorm(70.5, mean(x), sd(x))

#plot
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

#percentiles
summary(heights$height)

p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)

percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]

#define x and z
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x)


# calculate observed and theoretical quantiles
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))

# make QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# make QQ-plot with scaled values
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)


#ggplot create object
data(murders)

ggplot(data = murders)

murders %>% ggplot()

p <- ggplot(data = murders)
class(p)
p

murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y = total))

# add points layer to predefined ggplot object
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total))


# add text layer to scatterplot
p + geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label = abb))

# change the size of the points
p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb))

# move text labels slightly to the right
p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1)

# simplify code by adding global aesthetic
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) +
  geom_text(nudge_x = 1.5)

# local aesthetics override global aesthetics
p + geom_point(size = 3) +
  geom_text(aes(x = 10, y = 800, label = "Hello there!"))

# redefine p to be everything except the points layer
p <- murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

# make all points blue
p + geom_point(size = 3, color = "blue")

# color points by region
p + geom_point(aes(col = region), size = 3)

# define average murder rate
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

# basic line with average murder rate for the country, slope = 1
# change line to dashed and dark grey, line under points
p <- p + 
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  scale_color_discrete(name = "Region")    # capitalize legend title

# theme used for graphs in the textbook and course
library(dslabs)
ds_theme_set()

# themes from ggthemes
library(ggthemes)
p + theme_economist()    # style of the Economist magazine
p + theme_fivethirtyeight()    # style of the FiveThirtyEight website

# make the plot, combining all elements
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()
  

# define p
p <- heights %>%
    filter(sex == "Male") %>%
    ggplot(aes(x = height))
    
# basic histograms
p + geom_histogram()
p + geom_histogram(binwidth = 1)

# histogram with blue fill, black outline, labels and title
p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
    xlab("Male heights in inches") +
    ggtitle("Histogram")
	
# Code: Smooth density plots in ggplot2
p + geom_density()
p + geom_density(fill = "blue")


## group then add a density layer
heights %>% 
  ggplot(aes(height, group = sex))+
  geom_density()

## color instead of group then add a density layer
heights %>% 
  ggplot(aes(height, color = sex))+
  geom_density()

# Code: Quantile-quantile plots in ggplot2
# basic QQ-plot
p <- heights %>% filter(sex == "Male") %>%
    ggplot(aes(sample = height))
p + geom_qq()

# QQ-plot against a normal distribution with same mean/sd as data
params <- heights %>%
    filter(sex == "Male") %>%
    summarize(mean = mean(height), sd = sd(height))
    p + geom_qq(dparams = params) +
    geom_abline()
    
# QQ-plot of scaled data against the standard normal distribution
heights %>%
    ggplot(aes(sample = scale(height))) +
    geom_qq() +
    geom_abline()
# Code: Grids of plots with the gridExtra package
# define plots p1, p2, p3
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")

# arrange plots next to each other in 1 row, 3 columns
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)

data(gapminder)
head(gapminder)

# compare infant mortality in Sri Lanka and Turkey
gapminder %>%
    filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>%
    select(country, infant_mortality)
	
# basic scatterplot of life expectancy versus fertility    
# add color as continent
filter(gapminder, year == 1962) %>%
    ggplot(aes(fertility, life_expectancy, color = continent)) +
    geom_point()
	
# facet by continent and year
filter(gapminder, year %in% c(1962, 2012)) %>%
    ggplot(aes(fertility, life_expectancy, col = continent)) +
    geom_point() +
    facet_grid(continent ~ year)

# facet by year only
filter(gapminder, year %in% c(1962, 2012)) %>%
    ggplot(aes(fertility, life_expectancy, col = continent)) +
    geom_point() +
    facet_grid(. ~ year)

# facet by year, plots wrapped onto multiple rows
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>%
    filter(year %in% years & continent %in% continents) %>%
    ggplot(aes(fertility, life_expectancy, col = continent)) +
    geom_point() +
    facet_wrap(~year)
	
# scatterplot of US fertility by year
gapminder %>%
    filter(country == "United States") %>%
    ggplot(aes(year, fertility)) +
    geom_point()

# line plot of US fertility by year
gapminder %>%
    filter(country == "United States") %>%
    ggplot(aes(year, fertility)) +
    geom_line()
	
# line plot fertility time series for two countries- only one line (incorrect)
countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries) %>%
    ggplot(aes(year, fertility)) +
    geom_line()
    
# line plot fertility time series for two countries - one line per country
gapminder %>% filter(country %in% countries) %>%
    ggplot(aes(year, fertility, group = country)) +
    geom_line()

# fertility time series for two countries - lines colored by country
gapminder %>% filter(country %in% countries) %>%
    ggplot(aes(year, fertility, col = country)) +
    geom_line()
	
# life expectancy time series - lines colored by country and labeled, no legend
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>%
    ggplot(aes(year, life_expectancy, col = country)) +
    geom_line() +
    geom_text(data = labels, aes(x, y, label = country), size = 5) +
    theme(legend.position = "none")

# add dollars per day variable
gapminder <- gapminder %>%
    mutate(dollars_per_day = gdp/population/365)

# histogram of dollars per day
past_year <- 1970
gapminder %>%
    filter(year == past_year & !is.na(gdp)) %>%
    ggplot(aes(dollars_per_day)) +
    geom_histogram(binwidth = 1, color = "black")

# repeat histogram with log2 scaled data
gapminder %>%
    filter(year == past_year & !is.na(gdp)) %>%
    ggplot(aes(log2(dollars_per_day))) +
    geom_histogram(binwidth = 1, color = "black")
    
# repeat histogram with log2 scaled x-axis
gapminder %>%
    filter(year == past_year & !is.na(gdp)) %>%
    ggplot(aes(dollars_per_day)) +
    geom_histogram(binwidth = 1, color = "black") +
    scale_x_continuous(trans = "log2")
	
# add dollars per day variable
gapminder <- gapminder %>%
    mutate(dollars_per_day = gdp/population/365)
    
# number of regions
length(levels(gapminder$region))

# boxplot of GDP by region in 1970
past_year <- 1970
p <- gapminder %>%
    filter(year == past_year & !is.na(gdp)) %>%
    ggplot(aes(region, dollars_per_day))
p + geom_boxplot()

# rotate names on x-axis
p + geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
	
# by default, factor order is alphabetical
fac <- factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac)

# reorder factor by the category means
value <- c(10, 11, 12, 6, 4)
fac <- reorder(fac, value, FUN = mean)
levels(fac)

# reorder by median income and color by continent
p <- gapminder %>%
    filter(year == past_year & !is.na(gdp)) %>%
    mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%    # reorder
    ggplot(aes(region, dollars_per_day, fill = continent)) +    # color by continent
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("")
p

# log2 scale y-axis
p + scale_y_continuous(trans = "log2")

# add data points
p + scale_y_continuous(trans = "log2") + geom_point(show.legend = FALSE)

# add dollars per day variable and define past year
gapminder <- gapminder %>%
    mutate(dollars_per_day = gdp/population/365)
past_year <- 1970

# define Western countries
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

# facet by West vs devloping
gapminder %>%
    filter(year == past_year & !is.na(gdp)) %>%
    mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
    ggplot(aes(dollars_per_day)) +
    geom_histogram(binwidth = 1, color = "black") +
    scale_x_continuous(trans = "log2") +
    facet_grid(. ~ group)

# facet by West/developing and year
present_year <- 2010
gapminder %>%
    filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
    mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
    ggplot(aes(dollars_per_day)) +
    geom_histogram(binwidth = 1, color = "black") +
    scale_x_continuous(trans = "log2") +
    facet_grid(year ~ group)
	
# define countries that have data available in both years
country_list_1 <- gapminder %>%
    filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
    country_list_2 <- gapminder %>%
    filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
    country_list <- intersect(country_list_1, country_list_2)

# make histogram including only countries with data available in both years
gapminder %>%
    filter(year %in% c(past_year, present_year) & country %in% country_list) %>%    # keep only selected countries
    mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
    ggplot(aes(dollars_per_day)) +
    geom_histogram(binwidth = 1, color = "black") +
    scale_x_continuous(trans = "log2") +
    facet_grid(year ~ group)
	
p <- gapminder %>%
    filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
    mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
    ggplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("") + scale_y_continuous(trans = "log2")
    
 p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
     facet_grid(year ~ .)
 
 # arrange matching boxplots next to each other, colored by year
 p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))
 
  see the code below the previous video for variable definitions

# smooth density plots - area under each curve adds to 1
gapminder %>%
    filter(year == past_year & country %in% country_list) %>%
    mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>%
    summarize(n = n()) %>% knitr::kable()

# smooth density plots - variable counts on y-axis
p <- gapminder %>%
    filter(year == past_year & country %in% country_list) %>%
    mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
    ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
    scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)

# add group as a factor, grouping regions
gapminder <- gapminder %>%
    mutate(group = case_when(
            .$region %in% west ~ "West",
            .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
            .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
            .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
            TRUE ~ "Others"))

# reorder factor levels
gapminder <- gapminder %>%
    mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))
	
# note you must redefine p with the new gapminder object first
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
    ggplot(aes(dollars_per_day, fill = group)) +
    scale_x_continuous(trans = "log2")

# stacked density plot
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
    facet_grid(year ~ .)
	
# weighted stacked density plot
gapminder %>%
    filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
    group_by(year) %>%
    mutate(weight = population/sum(population*2)) %>%
    ungroup() %>%
    ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
    scale_x_continuous(trans = "log2") +
    geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)
	
# define gapminder
library(tidyverse)
library(dslabs)
data(gapminder)

# add additional cases
gapminder <- gapminder %>%
    mutate(group = case_when(
        .$region %in% west ~ "The West",
        .$region %in% "Northern Africa" ~ "Northern Africa",
        .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
        .$region == "Southern Asia" ~ "Southern Asia",
        .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
        .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
        .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

# define a data frame with group average income and average infant survival rate
surv_income <- gapminder %>%
    filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
    group_by(group) %>%
    summarize(income = sum(gdp)/sum(population)/365,
                        infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)

# plot infant survival versus income, with transformed axes
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
    scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
    scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                                       breaks = c(.85, .90, .95, .99, .995, .998)) +
    geom_label(size = 3, show.legend = FALSE) 
	
# dot plot showing the data
heights %>% ggplot(aes(sex, height)) + geom_point()

# jittered, alpha blended point plot
heights %>% ggplot(aes(sex, height)) + geom_jitter(width = 0.1, alpha = 0.2)

#color blind frendly colors
color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p1 <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
    ggplot(aes(x, y, color = col)) +
    geom_point(size = 5)
p1 + scale_color_manual(values = color_blind_friendly_cols)

#Code: Slope chart

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
    filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
    mutate(location = ifelse(year == 2010, 1, 2),
           location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                             location + 0.22, location),
           hjust = ifelse(year == 2010, 1, 0)) %>%
    mutate(year = as.factor(year)) %>%
    ggplot(aes(year, life_expectancy, group = country)) +
    geom_line(aes(color = country), show.legend = FALSE) +
    geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
    xlab("") +
    ylab("Life Expectancy") 
	
#Code: Bland-Altman plot

dat %>%
    mutate(year = paste0("life_expectancy_", year)) %>%
    select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
    mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
                difference = life_expectancy_2015 - life_expectancy_2010) %>%
    ggplot(aes(average, difference, label = country)) +
    geom_point() +
    geom_text_repel() +
    geom_abline(lty = 2) +
    xlab("Average of 2010 and 2015") +
    ylab("Difference between 2015 and 2010")
	
#Code: Tile plot of measles rate by year and state
# import data and inspect
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)

# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
    filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
    mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
    mutate(state = reorder(state, rate))

# plot disease rates per year in California
dat %>% filter(state == "California" & !is.na(rate)) %>%
    ggplot(aes(year, rate)) +
    geom_line() +
    ylab("Cases per 10,000") +
    geom_vline(xintercept=1963, col = "blue")

# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
    geom_tile(color = "grey50") +
    scale_x_continuous(expand = c(0,0)) +
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
    geom_vline(xintercept = 1963, col = "blue") +
    theme_minimal() + theme(panel.grid = element_blank()) +
    ggtitle(the_disease) +
    ylab("") +
    xlab("")
	
#Code: Line plot of measles rate by year and state
# compute US average measles rate by year
avg <- us_contagious_diseases %>%
    filter(disease == the_disease) %>% group_by(year) %>%
    summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)

# make line plot of measles rate by year by state
dat %>%
    filter(!is.na(rate)) %>%
    ggplot() +
    geom_line(aes(year, rate, group = state), color = "grey50", 
        show.legend = FALSE, alpha = 0.2, size = 1) +
    geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
    scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
    ggtitle("Cases per 10,000 by state") +
    xlab("") +
    ylab("") +
    geom_text(data = data.frame(x = 1955, y = 50),
        mapping = aes(x, y, label = "US average"), color = "black") +
    geom_vline(xintercept = 1963, col = "blue")


#Titanic assignment
#Question 2
titanic %>%
  ggplot(aes(Age, fill = Sex)) +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack")

titanic %>%
  ggplot(aes(Age)) +
  geom_density(bw=1.5)

summary(titanic$Sex)

tit_in <- titanic %>% group_by(Sex) %>% filter(Age>=18 & Age <=35) %>% 
  summarize(Anzahl = length(Sex)) %>% .$Anzahl
tit_all <- titanic %>% group_by(Sex) %>%  
  summarize(Anzahl = length(Sex)) %>% .$Anzahl
tit_in / tit_all

tit_in <- titanic %>% group_by(Sex) %>% filter(Age<17) %>% 
  summarize(Anzahl = length(Sex)) %>% .$Anzahl
tit_in / tit_all

tit_all <- titanic %>% filter(!is.na(Age)) %>% pull(Age)
titanic %>% filter(Age==max(tit_all))

#Question 3
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>% filter(!is.na(Age)) %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()

#Question 4
titanic %>% 
  ggplot(aes(Sex)) +
  geom_bar() +
  facet_grid(. ~ Survived)

#Question 5
titanic %>% filter(!is.na(Age) & !is.na(Survived)) %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2, bw=2)

#Question 6
titanic %>% filter(!is.na(Fare) & Fare>100) %>%
  ggplot(aes(Survived,Fare)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.2)
  
#Question 7
titanic %>% 
  ggplot(aes(Pclass, fill=Survived)) +
  geom_bar() 

titanic %>% 
  ggplot(aes(Pclass, fill=Survived)) +
  geom_bar(position = position_fill()) 

titanic %>% 
  ggplot(aes(Survived, fill=Pclass)) +
  geom_bar(position = position_fill()) 

#Question 8
titanic %>% filter(!is.na(Age) & !is.na(Survived)) %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2, bw=2) +
  facet_grid(Pclass ~ Sex)
  
titanic %>% filter(!is.na(Age) & !is.na(Survived)) %>%
  ggplot(aes(Age, y = ..count..)) +
  geom_density(alpha = 0.2, bw=2) +
  facet_grid(Pclass ~ .)


## Properties of Stars Exercises

#mean and sd
data(stars)
options(digits = 3)   # report 3 significant digits
fav_stats(stars$magnitude)


#density plot magnitude
stars %>%  
  ggplot(aes(magnitude))+
  geom_density()

#density plot temp
stars %>%  
  ggplot(aes(temp))+
  geom_density()

#scatter plot
stars %>%  
  ggplot()+
  geom_point(aes(temp, magnitude))

#reverse scales and log magnitude)
stars %>%  
  ggplot()+
  geom_point(aes(temp, magnitude))+
  scale_x_log10() +
  scale_x_reverse()+
  scale_y_reverse()

#add labels
stars %>% filter(temp<5000) %>%  
  ggplot(aes(x=temp, y=magnitude,label=star))+
  geom_point()+
  scale_x_log10() +
  scale_x_reverse()+
  scale_y_reverse()+
  geom_label(size = 3, show.legend = FALSE) 

#add color
stars %>% filter(temp >30000) %>%
  ggplot(aes(x=temp, y=magnitude, color=type))+
  geom_point()+
  scale_x_log10() +
  scale_x_reverse()+
  scale_y_reverse()+
  scale_colour_manual(values = c("#000000", "#AAAAAA", "#0022BB", "#22BB00", "#CCCCCC", "#CC00CC", "#CCCC00","#006600","#660066","#666600"))


## Climate change assessment
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

#works
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

#does not worl
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(year)

#works
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

#works
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

#does not worl
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(.$year)

#min
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  min()

temp_carbon %>%
  filter(year==1751) %>%
  pull(carbon_emissions) 
temp_carbon %>%
  filter(year==2014) %>%
  pull(carbon_emissions) 
9855/3

#temp_anomaly
temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  pull(year) %>%
  min()

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  pull(year) %>%
  max()

temp_carbon %>%
  filter(year==1880) %>%
  pull(temp_anomaly) 
temp_carbon %>%
  filter(year==2018) %>%
  pull(temp_anomaly) 
.82+.11


#time series plot

p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line()
p <- p + geom_hline(aes(yintercept = 0), col = "blue")  
p <- p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")
p 

#question 6
temp_carbon %>%
  filter(year > 1935 & year < 1940) %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() + 
  geom_hline(aes(yintercept = 0), col = "blue") + 
  ylab("Temperature anomaly (degrees C)") 

temp_carbon %>%
  filter(year > 1974 & year < 1980) %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() + 
  geom_hline(aes(yintercept = 0), col = "blue") + 
  ylab("Temperature anomaly (degrees C)") 

temp_carbon %>%
  filter(year > 1995 & year < 1998) %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() + 
  geom_hline(aes(yintercept = 0), col = "blue") + 
  ylab("Temperature anomaly (degrees C)") 


#question 7
temp_carbon %>%
  filter(year > 1880 & year < 2025) %>%
  filter(!is.na(temp_anomaly)) %>%
  filter(!is.na(ocean_anomaly)) %>%
  filter(!is.na(land_anomaly)) %>%
  ggplot() +
  geom_line(aes(year, temp_anomaly),col="red") + 
  geom_line(aes(year, ocean_anomaly),col="green") + 
  geom_line(aes(year, land_anomaly),col="brown") + 
  geom_hline(aes(yintercept = 0), col = "blue") + 
  ylab("Temperature anomaly (degrees C)") 

#question 8
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)


greenhouse_gases %>%
  ggplot(aes(y=concentration, x=year)) +
  geom_line() +
  facet_grid(gas ~ . , scales = "free") +
    geom_vline(aes(xintercept = 1850))  +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

#question 9
greenhouse_gases %>% filter(year >1820) %>%
  ggplot(aes(y=concentration, x=year)) +
  geom_line() +
  facet_grid(gas ~ . , scales = "free") +
  geom_vline(aes(xintercept = 1850))  +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

#question 10
temp_carbon %>%
  ggplot(aes(y=carbon_emissions, x=year)) +
  geom_line()

temp_carbon %>% filter(year>1975) %>%
  ggplot(aes(y=carbon_emissions, x=year)) +
  geom_line()

#question 11
co2_time <- historic_co2 %>%
  ggplot(aes(y=co2, x=year)) +
  geom_line( aes(color = source))
co2_time

#question 12
historic_co2 %>% filter( year < 2019 & year > -3000) %>%
  ggplot(aes(y=co2, x=year)) +
  geom_line( aes(color = source))

