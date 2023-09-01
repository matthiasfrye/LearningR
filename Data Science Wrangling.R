
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

##Strings
# read in raw murders data from Wikipedia
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

# inspect data and column classes
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)

s <- "Hello!"    # double quotes define a string
s <- 'Hello!'    # single quotes define a string
s <- `Hello`    # backquotes do not

#s <- "10""    # error - unclosed quotes
s <- '10"'    # correct

# cat shows what the string actually looks like inside R
cat(s)

s <- "5'"
cat(s)

# to include both single and double quotes in string, escape with \
#s <- '5'10"'    # error
#s <- "5'10""    # error
s <- '5\'10"'    # correct
cat(s)
s <- "5'10\""    # correct
cat(s)


# murders_raw was defined in the web scraping section

# detect whether there are commas
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas)) #deprecated
murders_raw %>% summarize_all(list(commas))
murders_raw %>% summarize_all(list(~commas(.)))


# replace commas with the empty string and convert to numeric
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)

# parse_number also removes commas and converts to numeric
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2)

# using mutate_at() 
murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head

# using across() 
murders_new <- murders_raw %>% mutate(across(2:3, parse_number)) 
murders_new %>% head

murders_new <- murders_raw %>% mutate_all(parse_number)
head(murders_new)

murders_raw %>% mutate(population= str_replace_all(population, c(","), ""))
murders_raw %>% mutate_at(2:3,list(~ str_replace_all(., c(","), "")))
murders_raw

##Heights example

#understand the problem
# load raw heights data and inspect
library(dslabs)
data(reported_heights)
class(reported_heights$height)

# convert to numeric, inspect, count NAs
x <- as.numeric(reported_heights$height)
head(x)
sum(is.na(x))

# keep only entries that result in NAs
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% 
  head(n=10)

# calculate cutoffs that cover 99.999% of human population (male and female separate)
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9)
qnorm(alpha/2, 63.7, 2.7)

#use the reported data:not feasible!
reported_heights %>% mutate(x=as.numeric(height)) %>% filter(!is.na(x) & sex=="Male") %>% summarize(mean(x))
reported_heights %>% mutate(x=as.numeric(height)) %>% filter(!is.na(x) & sex=="Male" & x>100) %>% head() 

# keep only entries that either result in NAs or are outside the plausible range of heights
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

# number of problematic entries
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
length(problems)

# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
cat(pattern)
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
cat(pattern)
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat


# learning regex

# detect whether a comma is present
pattern <- ","
str_detect(murders_raw$total, pattern) 
murders_raw$total[str_detect(murders_raw$total, pattern)]

# show the subset of strings including "cm"
str_subset(reported_heights$height, "cm")

# use the "or" symbol inside a regex (|)
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")

# highlight the first occurrence of a pattern
str_view(s, pattern)

# highlight all instances of a pattern
str_view_all(s, pattern)

##more regex
# s was defined in the previous video
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"

# [56] means 5 or 6
str_view(s, "[56]")

# [4-7] means 4, 5, 6 or 7
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
s
str_detect(s, "[4-7]")

# ^ means start of string, $ means end of string
pattern <- "^\\d$"
cat(pattern)
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)

# curly braces define quantifiers: 1 or 2 digits 
pattern <- "^\\d{1,2}$"
cat(pattern)
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)
str_view_all(c(yes, no), pattern)

# combining character class, anchors and quantifier
pattern <- "^[4-7]'\\d{1,2}\"$"
cat(pattern)
yes <- c("5'7\"", "6'2\"",  "5'12\"")
cat(yes)
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
cat(no)
str_detect(yes, pattern)
str_detect(no, pattern)

# number of entries matching our desired pattern
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))

# inspect examples of entries with problems
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
str_subset(problems, "inches")
str_subset(problems, "''")

# replace or remove feet/inches words before matching
pattern <- "^[4-7]'\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

# R does not ignore whitespace
identical("Hi", "Hi ")

# \\s represents whitespace
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)

# * means 0 or more instances of a character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")

# test how *, ? and + differ
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))

# update pattern by adding optional spaces before and after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

# define regex with and without groups
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"

# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)

# demonstrate the effect of groups
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)

# demonstrate difference between str_match and str_extract
str_match(s, pattern_with_groups)
str_extract(s, pattern_with_groups)

# improve the pattern to recognize more events
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

# final pattern
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

# combine stringr commands with the pipe
str_subset(problems, pattern_with_groups) %>% head
str_subset(problems, pattern_with_groups) %>% 
  str_replace(pattern_with_groups, "\\1'\\2") %>% head


##where are we now
# function to detect entries with problems
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

# identify entries with problems
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format

# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

converted[!index]    # show problems


#assessment
s <- c("70",       "5 ft",     "4'11",     "",         ".",        "Six feet")
s
  str_view_all(s,"\\d|ft")
  str_match(s,"\\d\\d|ft")
  str_subset(s,"\\d|ft")
 
schools <- c("U. Kentucky",                 "Univ New Hampshire",          "Univ. of Massachusetts",      "University Georgia",         
             "U California",                "California State University")
schools
schools %>% str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ")


problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
cat(pattern_with_groups)
str_replace(problems, pattern_with_groups, "\\1'\\2")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

str_detect("19","[193+]")


##Separate and Extract
# first example - normally formatted heights
s <- c("5'10", "6'1")
tab <- data.frame(x = s)

# the separate and extract functions behave similarly
tab %>% separate(x, c("feet", "inches"), sep = "'")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# second example - some heights with unusual formats
s <- c("5'10", "6'1\"","5'8inches")
cat(s)
tab <- data.frame(x = s)

# separate fails because it leaves in extra characters, but extract keeps only the digits because of regex groups
tab %>% separate(x, c("feet","inches"), sep = "'", fill = "right")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

#continue with reg ex
## final code
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}
words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}
converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]



pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

#check
new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

#map
# read raw murders data line by line
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()

# split at commas with str_split function, remove row of column names
x <- str_split(lines, ",") 
x %>% head()
col_names <- x[[1]]
x <- x[-1]

# extract first element of each list entry
library(purrr)
map(x, function(y) y[1]) %>% head()
map(x, 1) %>% head()

# extract columns 1-5 as characters, then convert to proper format - NOTE: DIFFERENT FROM VIDEO
dat <- data.frame(parse_guess(map_chr(x, 1)),
                  parse_guess(map_chr(x, 2)),
                  parse_guess(map_chr(x, 3)),
                  parse_guess(map_chr(x, 4)),
                  parse_guess(map_chr(x, 5))) %>%
  setNames(col_names)
str(dat)
dat <- data.frame(map_chr(x, 1),
                  map_chr(x, 2),
                  map_chr(x, 3),
                  map_chr(x, 4),
                  map_chr(x, 5)) %>%
  mutate_all(parse_guess) %>%
setNames(col_names)
str(dat)
dat %>% head

# more efficient code for the same thing
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>% 
  as.data.frame() 

# the simplify argument makes str_split return a matrix instead of a list
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)


##case study
library(dslabs)
data("research_funding_rates")
research_funding_rates 

##load PDF and convert into text
library(pdftools)
temp_file <- tempfile()
url <- "https://www.pnas.org/action/downloadSupplement?doi=10.1073%2Fpnas.1510159112&file=pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

#get raw data from table in PDF
raw_data_research_funding_rates <- txt[2]
raw_data_research_funding_rates %>% head

#use newlines "\n" as separators
tab <- str_split(raw_data_research_funding_rates, "\n")
str(tab)
#list of 1
tab <- tab[[1]]
tab %>% head

#work on colmn names
the_names_1 <- tab[3]
the_names_2 <- tab[4]


the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>% #cut-off after comma
  str_split("\\s{2,}", simplify = TRUE) #Split at two or more spaces
the_names_1

the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_") # names_1 wird je dreimal mit den Spaltennamne verknüpft
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names


## look at data
tab
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>% #Split at two or more spaces
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()

## recoding
# life expectancy time series for Caribbean countries

data("gapminder")
gapminder %>% 
  filter(region=="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# display long country names
gapminder %>% 
  filter(region=="Caribbean") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country) 

# recode long country names and remake plot
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic' = "Dom. Rep.",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()


#exercises
wday <- c("Monday", "Tusday")
staff <- c("Mandy, Chris and Laura","Steve, Ruth and Frank")
schedule <- data.frame(wday,staff)

tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest(cols=c(staff))
tidy         


str_detect("19.5","[1-9]*\\.5")


#btrexit
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[6]] %>% html_table(fill = TRUE)

polls <- polls %>% filter(str_detect(Remain,"%$")) %>%
  set_names(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes"))
polls$remain <- parse_number(polls$remain)/100
polls$undecided <- str_replace(polls$undecided, "N/A" , "0%")

str_extract_all("16 May-12 June", "\\d{1,2}\\s[a-zA-Z]+$")
str_extract_all("16 May-12 June", "\\d?\\s[a-zA-Z]?")
str_extract_all("16 May-12 June", "\\d+\\s[a-zA-Z]+")
str_extract_all("16 May-12 June", "\\d+\\s[A-Z]+")
str_extract_all("16 May-12 June", "[0-9]+\\s[a-zA-Z]+")
str_extract_all("16 May-12 June", "\\d{1,2}\\s[a-zA-Z]+")
str_extract_all("16 May-12 June", "\\d{1,2}[a-zA-Z]+")
str_extract_all("16 May-12 June", "\\d+\\s[a-zA-Z]{3,5}")


temp <- str_extract_all(polls$dates, "\\d{1,2}\\s[a-zA-Z]+$")
temp
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)



## WS Import

temp_file <- "/Users/matthias_frye/Downloads/p_wsi_report_86_2023.pdf"
txt <- pdf_text(temp_file)

#get raw data from table in PDF page 12
raw_data <- txt[12]
raw_data %>% head

#use newlines "\n" as separators
tab <- str_split(raw_data, "\n")
str(tab)

tab <- tab[[1]]
tab[11:44] %>% head(40)

tab<-c(tab[12:14], tab[17:23], tab[26:31], tab[34:44])


## look at data
new_data<- tab %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>% #Split at two or more spaces
  data.frame(stringsAsFactors = FALSE)%>%
  select(X1,X12)

write_excel_csv2(new_data,"/Users/matthias_frye/Downloads/p_wsi_report_86_2023.xls")



##dates and time
# inspect the startdate column of 2016 polls data, a Date type
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head

# ggplot is aware of dates
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

# lubridate: the tidyverse date package
library(lubridate)

# select some random dates from polls
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

# extract month, day, year from date strings
data.frame(date = dates, 
           month = month(dates),
           day = day(dates),
           year = year(dates))

month(dates, label = TRUE)    # extract month label

# ymd works on mixed date styles
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

now()    # current time in your time zone
now("GMT")    # current time in GMT
now() %>% hour()    # current hour
now() %>% minute()    # current minute
now() %>% second()    # current second

# parse time
x <- c("12:34:56")
hms(x)

#parse datetime
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)


## text mining
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)
#library(rtweet)

#gives error
url <- 'https://drive.google.com/file/d/16wm-2NTKohhcA26w-kaWfhLIGwl_oX95/view'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 

#but here it is
library(dslabs)
data("trump_tweets")
head(trump_tweets)
names(trump_tweets)
trump_tweets %>% select(text) %>% head
trump_tweets %>% count(source) %>% arrange(desc(n))

trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source) 

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

library(tidytext)
example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

#a patter that starts with @, # or neither and is followed by any combination of letters or digits:
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
cat(pattern)
campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#remove the links to pictures:
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#extract the words
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 

#count(words)
tweet_words %>% 
  count(word) %>%
  arrange(desc(n))

tweet_words %>% 
  count(word) %>%
  arrange(desc(n)) %>% head()

#filter out stop words
stop_words
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) 

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

#remove years and quotes
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or)) %>% head
android_iphone_or %>% arrange(or)%>% head


android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)

sentiments 
get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)

#combine tweet and sentiment
tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% sample_n(10)

sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

tweet_words %>% group_by(source) %>% summarize(n = n())

sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 

android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))

android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

############# 

## exercises
library(dslabs)
library(lubridate)
options(digits = 3) 


data(brexit_polls)
sum(month(brexit_polls$startdate)==4)
sum(round_date(brexit_polls$enddate,unit="week")==round_date(as.Date("2016-06-12"),unit="week"))

brexit_polls %>% group_by(weekdays(enddate)) %>% summarize(n=n()) %>% arrange(desc(n))

data(movielens)

movielens %>% group_by(year(as_datetime(timestamp))) %>% summarize(n=n()) %>% arrange(desc(n))
movielens %>% group_by(hour(as_datetime(timestamp))) %>% summarize(n=n()) %>% arrange(desc(n))


library(tidyverse)
library(tidytext)
options(digits = 3)

url <- "https://cran.r-project.org/src/contrib/Archive/gutenbergr/gutenbergr_0.2.3.tar.gz" 
rchive_gb <- "gutenbergr_0.2.3.tar.gz" 
download.file(url = url, destfile = rchive_gb) 
install.packages(pkgs=rchive_gb, type="source", repos=NULL) 
unlink(rchive_gb)
library(gutenbergr)

gutenberg_metadata      

gutenberg_metadata %>%  filter(str_detect(title, "Pride and Prejudice") )

gutenberg_works(languages="en")%>%  filter(str_detect(title, "Pride and Prejudice") )

x<-gutenberg_download(1342)
str(x)
words <- x %>% unnest_tokens(word, text, token = "words")
nrow(words)

#words %>% group_by(word) %>% summarize(n=n()) %>% arrange(desc(n))
words %>% filter(!word %in% stop_words$word ) %>% 
  filter(!str_detect(word,"\\d")) %>% nrow()
words %>% filter(!word %in% stop_words$word ) %>% 
  filter(!str_detect(word,"\\d")) 

words %>% filter(!word %in% stop_words$word ) %>% 
  filter(!str_detect(word,"\\d")) %>% 
  group_by(word) %>% summarize(n=n()) %>% arrange(desc(n))

afinn <- get_sentiments("afinn")
words <- words %>% filter(!word %in% stop_words$word ) %>% 
  filter(!str_detect(word,"\\d"))

words %>% inner_join(afinn, by = "word")  %>% count(word)

afinn_sentiments <- words %>% inner_join(afinn, by = "word")
alle  <- afinn_sentiments %>% nrow()
alle
posi <- afinn_sentiments %>% filter(value>0) %>% nrow()
posi / alle

afinn_sentiments %>% filter(value==4) %>% nrow()

####
library(tidyverse)
library(pdftools)
options(digits = 3) 

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system2("open", args = fn)

txt <- pdf_text(fn)
txt


#use newlines "\n" as separators
x <- str_split(txt[9], "\n")
str(x)
x
s<-x[[1]]
s
s<- s %>% str_trim()
s

head_index <- s%>% str_which("2015")
hi<-head_index[1]+1
hi
header <- s[head_index[1]]
header <- str_split(header,"\\s+", simplify = TRUE)
header
month <- header[1,1]
month
header <- header[1,-1]
header

tail_index <- s%>% str_which("Total")
ti<-tail_index[1]-1
ti
s
t <- s[hi:ti]
t
n <- t %>% str_count("\\d+")
n
sum(n==1)
t <- t[!n==1]
t

ct <- str_remove_all(t,"[^\\d\\s]")
ct

st <- str_split_fixed(ct, "\\s+", n = 6)[,1:5]

tab <-data.frame(st,stringsAsFactors = FALSE)  %>%
  setNames(c("day",header)) %>%
  mutate_all(parse_number)
tab
mean(tab$`2015`)
mean(tab$`2016`)
mean(tab$`2017`[1:19])
mean(tab$`2017`[20:30])

tab %>% filter(day <=19) %>% summarize(avg=mean(`2017`))
tab %>% filter(day >=20) %>% summarize(avg=mean(`2017`))


tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab

tab %>% filter(year != 2018) %>%
  ggplot(aes(x=day, y=deaths, color=year))  +
  geom_line() +
  geom_vline(xintercept=20)
