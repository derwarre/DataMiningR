#' ---
#' title: "Homework 3 - Olivieri"
#' output: html_notebook
#' ---
## ------------------------------------------------------------------------
library(ggplot2)
library(arules)
library(arulesViz)
library(dplyr)
library(plotly)

#' 
## ------------------------------------------------------------------------
### Your working directory will be different.
setwd("/Users/aoliv01/Desktop/GradSchool/2018-2/DataMining/Homework/HW3")
getwd()

#' 
## ------------------------------------------------------------------------
### Reading in the data
raw <- read.csv("bankdata_csv_all.csv")
str(raw)

## ------------------------------------------------------------------------
attach(raw)

#' 
## ------------------------------------------------------------------------
## Checking for null records
total_na <- sum(is.na(raw))
cat("Total na is: ", total_na)

#' 
## ------------------------------------------------------------------------
## Removing the id column
raw <- raw[,-1]

#' 
## ------------------------------------------------------------------------
## Quick boxplot to check for income outliers
income_box <- ggplot(raw, aes(x = pep, y = income)) + 
  geom_boxplot(fill = "steelblue", color = "darkorange") +
  theme(plot.title = element_text(hjust = 0.5, color = "darkorange"))

income_box

#' 
## ------------------------------------------------------------------------
## Box plot to check for age outliers or odd values
age_box <- ggplot(raw, aes(x = pep, y = age)) + 
  geom_boxplot(fill = "steelblue", color = "darkorange") +
  theme(plot.title = element_text(hjust = 0.5, color = "darkorange"))
age_box

#' 
## ------------------------------------------------------------------------
## Chart showing income and age of PEP customers and not PEP customers
ggplot(data = raw, aes(x = income, y = age)) + 
  geom_point(colour = "steelblue", alpha = .5) + 
  geom_smooth(method = "lm") +
  facet_wrap(~ pep) + 
  ggtitle("PEP by Age and Income")

#' 
## ------------------------------------------------------------------------
## Checking to see that age and income for PEP and non-PEP are comparable
## This will also show outliers
aggregate(income ~ pep, data = raw, median)
aggregate(age ~ pep, data = raw, median)
aggregate(income ~ pep, data = raw, max)
aggregate(age ~ pep, data = raw, max)
aggregate(income ~ pep, data = raw, min)
aggregate(age ~ pep, data = raw, min)

#' 
#' 
## ------------------------------------------------------------------------
## Discretizing record data into transactional data
change <- c("id", "age", "income", "children")
disc <- raw[ , !(names(raw) %in% change)]

disc$age <- discretize(
  raw$age,
  method = "cluster",
  labels = c("young", "middle", "older")
  )
disc$income <- discretize(
    raw$income,
    method = "cluster",
    labels = c("low", "medium", "high")
  )
disc$children <- discretize(
    raw$children,
    method = "cluster",
    labels = c("low", "medium", "many")
  )
detach(raw)
summary(disc)

#' 
#' 
## ------------------------------------------------------------------------
## Running apriori algorithm
rules <- apriori(disc, parameter = list(supp = 0.025, conf = 0.75, maxlen = 4))
rules<-sort(rules, by="confidence", decreasing=TRUE)
options(digits=2)
inspect(head(rules, 20))

#' 
## ------------------------------------------------------------------------
# Finding the highest ranked rules by support and confidence
# Writing a csv file of the rules
highest <- data.frame(
       lhs = labels(lhs(rules)),
       rhs = labels(rhs(rules)),
      rules@quality)
highest <- highest[which(highest$support > mean(highest$support) & highest$confidence > mean(highest$confidence) & highest$lift > 1),]
highest <- highest[order(highest$confidence, highest$support),]
highest <- highest[1:25,]
write.csv(highest, file = "BankRuleSets.csv")

#' 
#' 
## ------------------------------------------------------------------------
## The top rules for rhs = PEP
inspect(subset(rules, subset = support > mean(support) & confidence > mean(confidence) & lift > mean(lift) &  rhs %pin% "pep=YES" ))

#' 
## ------------------------------------------------------------------------
## All rules for rhs = PEP
inspect(subset(rules, subset = rhs %pin% "pep=YES" ))

#' 
## ------------------------------------------------------------------------
## Formatting a new dataset to plot
ruledf = data.frame(
       lhs = labels(lhs(rules)),
       rhs = labels(rhs(rules)), 
       rules@quality)
ruledf$pep <- ifelse(ruledf$rhs == "{pep=YES}", 'YesPep', 'NoPep')

## Plotting the new dataset in an interactive 3D plot
p <- plot_ly(ruledf,
              x = ~confidence,
              y = ~lift,
              z = ~support,
              color = ~ruledf$pep,
              colors = c('steelblue', 'darkorange'),
              marker = list(size = 4, opacity = 0.35)
             ) %>% 
    add_markers() %>%
    layout(scene = list(
                   xaxis = list(title = 'support'),
                   yaxis = list(title = 'confidence'),
                   zaxis = list(title = 'lift')
                   ))
p

#' 
#' 
