###Productivity

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

#load data
data(murders)

#add murder rate in millions
murders <- murders %>% mutate(rate=total/population*10^6)

murders %>% ggplot(aes(population,total, label=abb,color=region))+
  geom_label()


# scatter
murders %>% ggplot(aes(population,total))+
  geom_point()

# scatter + color
murders %>% ggplot(aes(population,total, color=region))+
  geom_point()
