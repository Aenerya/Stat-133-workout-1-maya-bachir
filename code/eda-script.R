# Title: eda-script
# Description: This script contains the exploratory data analysis of part 4 of the workout assignment.
# Input(s): data file 'ibtracs-2010-2015.csv'
# Output(s): summary data files, and plots
# Author(s): Maya Bachir
# Date: 10/18/2019

# packages
library(dplyr)    # data wrangling
library(ggplot2)  # graphics
library(tidyverse) #contains readr
library(maps) #maps
library(lubridate) #handling the date

#code
dat <- read.csv('C:/Users/Maya/Documents/Berkeley/Computing data/Workout1/data/ibtracs-2010-2015.csv', header = FALSE, sep = ";", skip = 2, colClasses = c('character','integer','character','factor','character','character','character','character','real','real','real', 'real'), na.string = c("-999.","-1.0", "0.0"), col.names = c('serial_num', 'season', 'num', 'bassin', 'sub_bassin', 'name', 'iso_time', 'nature', 'latitude', 'longitude', 'wind', 'press'))

sink(file = 'data-summary.txt')
summary(dat)
sink()

map("world", fill = T, col = "white", bg = "lightblue")
points(dat$longitude, dat$latitude, col = 'purple')
points(long <- seq(-180,180), lat <- rep(0, length(long)), col ='red', cex = 0.2)

dat$month = month(dat$iso_time, label=TRUE)
dat$year = year(dat$iso_time)

dat_EP_bassin <- filter(dat, dat$bassin == ' EP')
ggplot(dat_EP_bassin, aes(dat_EP_bassin$iso_time)) + geom_bar() + facet_grid(dat_EP_bassin$year)
ggplot(dat_EP_bassin, aes(dat_EP_bassin$iso_time)) + geom_bar() + facet_grid(dat_EP_bassin$month)

dat_NA_bassin <- filter(dat, dat$bassin == ' NA')
ggplot(dat_NA_bassin, aes(dat_NA_bassin$iso_time)) + geom_bar() + facet_grid(dat_NA_bassin$year)
ggplot(dat_NA_bassin, aes(dat_NA_bassin$iso_time)) + geom_bar() + facet_grid(dat_NA_bassin$month)
