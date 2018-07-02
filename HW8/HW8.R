#' ---
#' title: "HW8"
#' author: "Ollie"
#' date: "6/6/2018"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
## ------------------------------------------------------------------------
setwd("/Users/aoliv01/Desktop/GradSchool/2018-2/DataMining/Homework/HW8")

#' 
## ----include=FALSE-------------------------------------------------------
library(ggplot2) 
library(caret)   
library(readr)
library(farff)
library(plyr) ## load this BEFORE dplyr
library(dplyr)
library(lattice)
library(e1071)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(psych)
library(tm)
library(wordcloud)

#' 
## ------------------------------------------------------------------------
raw = readARFF("deception_data_converted_arff.arff")

#' 
## ------------------------------------------------------------------------
cat("NA's in raw: ", sum(is.na(raw)))

#' 
## ------------------------------------------------------------------------
clean = raw[complete.cases(raw),]

str(clean)

#' 
## ------------------------------------------------------------------------
corp = VCorpus(VectorSource(clean$review))

#' 
## ------------------------------------------------------------------------
review_prepped = DocumentTermMatrix(corp, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE))

#' 
## ------------------------------------------------------------------------
freq_rev <- review_prepped %>%
  findFreqTerms(5) %>%
  review_prepped[ , .]

#' 
## ------------------------------------------------------------------------
hot_encode = function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}
rev_all_prep_clean = freq_rev %>%
  apply(MARGIN = 2, hot_encode)

#' 
## ------------------------------------------------------------------------
clean_prepped = cbind(clean, rev_all_prep_clean)
clean_prepped = lapply(clean_prepped, unname)
clean_prepped = data.frame(clean_prepped)

#' 
## ------------------------------------------------------------------------
search_grid = expand.grid(usekernel = c(TRUE, FALSE), fL = 0:5, adjust = seq(0, 5, by = 1))
train_control = trainControl(method = "cv", number = 10)

#' 
#' 
## ----results="hidden"----------------------------------------------------
search_grid = expand.grid(usekernel = c(TRUE, FALSE), fL = 0:5, adjust = seq(0, 5, by = 1))
train_control = trainControl(method = "cv", number = 10)
label = "lie"
predictors = names(clean_prepped[-1])
#model_nb = train(clean_prepped[, predictors], clean_prepped[,label], method = "nb", trControl = train_control, tuneGrid = search_grid)

#' 
## ------------------------------------------------------------------------
plot(model_nb)
print(model_nb)

#' 
## ----include=FALSE-------------------------------------------------------
nb_pred = predict(model_nb, clean_prepped[,-1])

#' 
## ------------------------------------------------------------------------
confusionMatrix(nb_pred, clean_prepped$lie)

#' 
## ------------------------------------------------------------------------
rev_hot_encode = function(x) {
  x <- ifelse(x > 0, 1 , 0)
}
rev_all_prep_clean = freq_rev %>%
  apply(MARGIN = 2, rev_hot_encode)
clean_prepped = cbind(clean, rev_all_prep_clean)
clean_prepped = lapply(clean_prepped, unname)
clean_prepped = data.frame(clean_prepped)

#' 
## ------------------------------------------------------------------------
#, sigma = c(.01, .015, 0.2)
predictors = names(clean_prepped)[-1:-3]
grid <- expand.grid(C = c(0.75, 0.9, 1, 1.1, 1.25))
svm_model = train(lie ~., data = clean_prepped, trControl = train_control, method = "svmLinear", tuneGrid = grid, scale = FALSE)

#' 
## ------------------------------------------------------------------------
svm_pred = predict(model_svm, clean_prepped[,label])

#' 
## ------------------------------------------------------------------------
confusionMatrix(svm_pred, clean_prepped$lie)

