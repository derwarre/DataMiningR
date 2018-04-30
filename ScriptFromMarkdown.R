library(knitr)


setwd("/Users/aoliv01/Desktop/GradSchool/2018-2/DataMining/Classwork/Week4")
getwd()

purl("Week4.RMD", output = "Week4.R", documentation = 2)
