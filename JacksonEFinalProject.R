# EJackson
# Final Project

library(tidyverse)
w <- read.csv('WVS (2).csv')
w <- as.data.frame(w)
w <- w %>% select(-X)
str(w)
w$gender <- as.factor(w$gender)

wplot <- ggplot()

