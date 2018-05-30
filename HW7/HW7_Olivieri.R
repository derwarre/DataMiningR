#' ---
#' title: "HW7 - Olivieri"
#' author: Ollie
#' date: May 28, 2018
#' output: html
#' ---
#' 
#' 
## ------------------------------------------------------------------------
setwd("/Users/aoliv01/Desktop/GradSchool/2018-2/DataMining/Homework/HW7")
getwd()

#' 
## ------------------------------------------------------------------------
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(e1071)
library(class)
library(randomForest)
library(caret)
library(readr)

#' 
## ------------------------------------------------------------------------
img_train <- read_csv("train.csv")
img_test <- read_csv("test.csv")

#' 
## ------------------------------------------------------------------------
cat("\nNA's train: ", sum(is.na(img_train)))
cat("\nNA's test: ", sum(is.na(img_test)))

cat("\nNegative values train: ", sum(img_train < 0))
cat("\nNegative values test: ", sum(img_test < 0))
cat("\nMax Values train / test: \n")
max(img_train)
max(img_test)

#' 
## ------------------------------------------------------------------------
knn_cut_index <- sample(seq_len(nrow(img_train)), size = 1000)
knn_cut <- img_train[knn_cut_index, ]
split <- floor(dim(knn_cut)[1] * .7)
set.seed(123)
knn_build_index <- sample(seq_len(nrow(knn_cut)), size = split)
knn_build <- knn_cut[knn_build_index, ]
knn_verify <- knn_cut[-knn_build_index, ]
train_control <- trainControl(method="repeatedcv", number = 5, repeats = 3)

#' 
## ------------------------------------------------------------------------
knn_build$label <- factor(knn_build$label)
knn_fit <- train(label~ ., data = knn_build, trControl = train_control, method = "knn")

plot(knn_fit)

#' 
## ------------------------------------------------------------------------
knn_pred <- predict(knn_fit, knn_verify)

#' 
#' 
## ------------------------------------------------------------------------
confusionMatrix(knn_pred, knn_verify$label)$overall['Accuracy']
confusionMatrix(knn_pred, knn_verify$label)$table

#' 
## ------------------------------------------------------------------------
knn_test_pred <- predict(knn_fit, img_test)

#' 
## ------------------------------------------------------------------------
knn_df <- data.frame(ImageId = seq.int(length(knn_test_pred)), Label = knn_test_pred)
head(knn_df)
tail(knn_df)
cat("\nNA's: ", sum(is.na(knn_df)))

#' 
#' 
## ------------------------------------------------------------------------
write.csv(knn_df, file='submission_knn.csv', row.names=FALSE,  quote=FALSE)

#' 
#' 
## ------------------------------------------------------------------------
svm_fit <- train(label~ ., data = knn_build, trControl = train_control, method = "svmLinear")


## ------------------------------------------------------------------------
svm_pred <- predict(svm_fit, knn_verify)
confusionMatrix(svm_pred, knn_verify$label)$overall['Accuracy']
confusionMatrix(svm_pred, knn_verify$label)$table

#' 
## ------------------------------------------------------------------------
svm_test_pred <- predict(svm_fit, img_test)

#' 
#' 
## ------------------------------------------------------------------------
svm_df <- data.frame(ImageId = seq.int(length(svm_test_pred)), Label = svm_test_pred)
head(svm_df)
tail(svm_df)
cat("\nNA's: ", sum(is.na(svm_df)))

#' 
## ------------------------------------------------------------------------
write.csv(svm_df, file='submission_svm.csv', row.names=FALSE,  quote=FALSE)

#' 
## ------------------------------------------------------------------------
rf_fit <- train(label~ ., data = knn_build, trControl = train_control, method = "rf")


## ------------------------------------------------------------------------
rf_pred <- predict(rf_fit, knn_verify)
confusionMatrix(rf_pred, knn_verify$label)$overall['Accuracy']
confusionMatrix(rf_pred, knn_verify$label)$table

#' 
## ------------------------------------------------------------------------
rf_test_pred <- predict(rf_fit, img_test)

#' 
## ------------------------------------------------------------------------
rf_df <- data.frame(ImageId = seq.int(length(rf_test_pred)), Label = rf_test_pred)
head(rf_test_pred)
tail(rf_test_pred)
cat("\nNA's: ", sum(is.na(rf_test_pred)))

#' 
## ------------------------------------------------------------------------
write.csv(rf_df, file='submission_df.csv', row.names=FALSE,  quote=FALSE)

#' 
#' 
#' 
#' 
#' 
