
library(tidyverse) #load also ggplot2
library(dslabs)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(titanic)
library(gtools)
library(rvest)

library(mosaic) # Diesen Befehl bei jeder Session am Anfang ausführen
library(readr)
library(gplots)
library(readxl)

## Section 1
# see working directory
getwd()

# change your working directory
setwd()

# set path to the location for raw data files in the dslabs package and list files
path <- system.file("extdata", package="dslabs")
path
list.files(path)

# generate a full path to a file
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath

# copy file from dslabs package to your working directory
file.copy(fullpath, getwd())

# check if the file exists
file.exists(filename)


## Section 2
# inspect the first 3 lines
read_lines("murders.csv", n_max = 3)

# read file in CSV format with tidyverse function creating tibble
dat <- read_csv(filename)

#read using full path
dat <- read_csv(fullpath)
head(dat)

#Exercise：
path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files

filename <- "murders.csv"
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat=read.csv(file.path(path, filename))
dat1=read.csv(file.path(path, filename1))
dat2=read.csv(file.path(path, filename2))
# filename is defined in the previous video

# read.csv to import the data - R base function creates data frame
dat2 <- read.csv(filename)
class(dat2$abb)
class(dat2$region)

#load from URL
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)

#download to tmpfile from URL
download.file(url, "murders.csv")
tempfile()
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)



#exercise 1
filename <- "murders.csv"
path <- system.file("extdata", package = "dslabs")
file.copy(file.path(path, "murders.csv"), getwd())
file.path(path, "murders.csv")
getwd()


url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
dat <- read_csv(url, col_names = FALSE)
nrow(dat)
ncol(dat)

#section 2

library(tidyverse)
library(dslabs)
data(gapminder)

# create and inspect a tidy data frame
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

# plotting tidy data is simple
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# import and inspect example of original Gapminder data in wide format
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)

# move the values in the columns 1960 through 2015 into a single column
wide_data %>% pivot_longer(`1960`:`2015`)

# another way to do this - only country isn't being pivoted
wide_data %>% pivot_longer(-country)

# change the default column names
new_tidy_data <- wide_data %>% 
  pivot_longer(-country, names_to = "year", values_to = "fertility")
head(new_tidy_data)

# compare the class from our original tidy data (year is an integer) and in the new version (year is a character)
class(tidy_data$year)
class(new_tidy_data$year)

# use the names_transform argument to change the class of the year values to numeric
new_tidy_data <- wide_data %>% 
  pivot_longer(-country, names_to = "year", values_to = "fertility", 
               names_transform = list(year=as.numeric))
class(new_tidy_data$year)
#could have done:
#new_tidy_data <- new_tidy_data %>% mutate(year=as.numeric(year))
#new_tidy_data$year <- as.numeric(new_tidy_data$year)
#new_tidy_data$year <- as.integer(new_tidy_data$year)

# plot the data as before
new_tidy_data %>% ggplot(aes(year, fertility, color = country)) +
  geom_point()

# still working with the same data as in the previous video
# convert the tidy data to wide data
new_wide_data <- new_tidy_data %>% 
  pivot_wider(names_from = year, values_from = fertility)
select(new_wide_data, country, `1960`:`1967`)




# import data
path <- system.file("extdata", package = "dslabs")
fname <-  "life-expectancy-and-fertility-two-countries-example.csv"
filename <- file.path(path, fname)

raw_dat <- read_csv(filename)
select(raw_dat, 1:4)

# pivot all columns except country
dat <- raw_dat %>% pivot_longer(-country)
head(dat)
dat$name[1:5]

# separate on underscores
dat %>% separate(name, c("year", "name"), sep = "_")

# separate on underscores (the default), convert years to numeric
dat %>% separate(name, c("year", "name"), convert = TRUE)

# split on all underscores, pad empty cells with NA
dat %>% separate(name, c("year", "name_1", "name_2"), 
                 fill = "right", convert = TRUE)

# split on first underscore but keep life_expectancy merged
dat %>% separate(name, c("year", "name"), sep = "_", 
                 extra = "merge", convert = TRUE)

# separate then create a new column for each variable using pivot_wider
dat %>% separate(name, c("year", "name"), sep = "_", 
                 extra = "merge", convert = TRUE) %>%
  pivot_wider()


##example unite
# using the data from the previous video
# if we had used this non-optimal approach to separate
dat %>% 
  separate(name, c("year", "name_1", "name_2"), 
           fill = "right", convert = TRUE)

# we could unite the second and third columns using unite()
dat %>% 
  separate(name, c("year", "name_1", "name_2"), 
           fill = "right", convert = TRUE) %>%
  unite(variable_name, name_1, name_2, sep="_")

# spread the columns
dat %>% 
  separate(name, c("year", "name_1", "name_2"), 
           fill = "right", convert = TRUE) %>%
  unite(name, name_1, name_2, sep="_") %>%
  spread(name, value) %>%
  rename(fertility = fertility_NA)

#could have used pivot_wider()
dat %>% 
  separate(name, c("year", "name_1", "name_2"), 
           fill = "right", convert = TRUE) %>%
  unite(name, name_1, name_2, sep="_") %>%
  pivot_wider() %>%
  rename(fertility = fertility_NA)


#Exercise
co2
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_wide
co2_tidy <- co2_wide %>% pivot_longer(1:12, names_to = "month", values_to = "co2")
co2_tidy

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()


data(admissions)
dat <- admissions %>% select(-applicants)
dat
dat_tidy <- dat %>% pivot_wider(names_from = gender, values_from = admitted)
dat_tidy


tmp <- admissions %>%
  pivot_longer(cols = c(admitted, applicants), names_to = "key", values_to = "value")
tmp

tmp2 <- tmp %>% unite(column_name, c(key, gender), sep="_")
tmp2 %>% pivot_wider()
tmp2 %>% pivot_wider(names_from = column_name, values_from = value)


#Joining
# import US murders data
library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)

# join the murders table and US election results table
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

# plot electoral votes versus population
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

# make two smaller tables to demonstrate joins
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2

# experiment with different joins
left_join(tab1, tab2)
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
inner_join(tab1, tab2)
semi_join(tab1, tab2)
anti_join(tab1, tab2)

#Binding
bind_cols(a = 1:3, b = 4:6)

tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2)

#Set functions
# intersect vectors or data frames
intersect(1:10, 6:15)
intersect(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)

# perform a union of vectors or data frames
union(1:10, 6:15)
union(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)

# set difference of vectors or data frames
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1, tab2)

# setequal determines whether sets have the same elements, regardless of order
setequal(1:5, 1:6)
setequal(1:5, 5:1)
setequal(tab1, tab2)


##question 5-7
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
People %>% as_tibble()

top_names <- top %>% left_join(People) %>%
  select(playerID, nameFirst, nameLast, HR)
top_names
Salaries %>% filter(yearID == 2016)
top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

a16 <- AwardsPlayers %>% filter(yearID == 2016)  %>% select(playerID)
t<- top %>% mutate(p=playerID) %>% select(p) 
t
intersect(t,a16)
nrow(setdiff(a16,t))


##Web Scraping
# import a webpage into R
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
url <- "https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

tab <- h %>% html_nodes("table")
tab #show all tables on pahe
tab <- tab[[3]]

tab <- tab %>% html_table()
class(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)


#Assessment
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")
sapply(nodes[1:4], html_table) 
tab_1 <- html_table(nodes[10] ) %>% data.frame() 
tab_1
tab_2 <- html_table(nodes[19] ) %>% data.frame() %>% setNames(c("Team", "Payroll", "Average")) 
tab_2 <- tab_2[-1,]
tab_2

tt <- full_join(tab_1,tab_2,by="Team")
nrow(tt)


url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
class(h)
tab <- h %>% html_nodes("table")
tab
length(tab)

html_table(tab[4], fill=TRUE)
html_table(tab[6], fill=TRUE)
