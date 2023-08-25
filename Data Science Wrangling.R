
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
