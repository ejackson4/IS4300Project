# EJackson
# Final Project

library(tidyverse)
library(ggthemes)
urlfile <- "https://raw.githubusercontent.com/ejackson4/IS4300Project/main/WVS%20(2).csv"
# Read World Values Survey data into R and name it w
w <- read_csv(url(urlfile))
w <- as.data.frame(w)
w <- w %>% select(-...1) # removing ID column

# Converting character datatypes to factors to increase visibility in summary function
w$gender <- as.factor(w$gender)
w$poverty <- as.factor(w$poverty)
w$religion <- as.factor(w$religion)
w$degree <- as.factor(w$degree)
w$country <- as.factor(w$country)

w <- w %>% mutate(ageCategories = case_when(age <=25 ~ "Gen Z", 
                                            age >= 26 & age < 40 ~ "Millennial",
                                            age >= 40 & age <= 56 ~ "Gen X",
                                            age > 56 ~ "Baby Boomers")) # Creating categorical data to analyze variables with chi squared test.
w$ageCategories <- as.factor(w$ageCategories)
summary(w) 


# Visualization

mosaicplot(poverty ~ country, data = w, col = c("navy", "ivory", "gold", "firebrick"), 
           main = "Poverty as viewed by Country", xlab = "Poverty", ylab = "Country")

wplot <- w %>% ggplot()


wplot + geom_bar(aes(x = religion, fill = poverty), width = .4) + theme_minimal() +
  labs(title = "View of poverty by religion")

wplot + geom_bar(aes(x = degree, fill = poverty), width = .4) + theme_minimal() + 
  labs(title ="View of poverty by degree")

wplot + geom_boxplot(aes(age, poverty)) + theme_minimal() + labs(title = "View of poverty by age")

wplot + geom_bar(aes(x = gender, fill = poverty), width = .4) + theme_minimal() + 
  labs(title = "View of poverty by gender")

# Analysis 
table1 <- table(w$poverty, w$country)
table1
chisq.test(table1)
table2 <- table(w$poverty, w$religion)
table2
chisq.test(table2)
table3 <- table(w$poverty, w$degree)
table3
chisq.test(table3)
table4 <- table(w$poverty, w$gender)
table4
chisq.test(table4)
table5 <- table(w$poverty, w$ageCategories)
chisq.test(table5)
