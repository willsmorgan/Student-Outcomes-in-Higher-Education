#### SVM workspace


#------------------------------------------------------------------------------#
rm(list = ls())
set.seed(1)

library('e1071')

### practice area
dv <- 'aorbetter'
family <- 'binomial'

source("Code/Model Prep.R", local = TRUE)

X_train <- X_train[[1]]
Y_train <- Y_train[[1]] %>%
  as.factor()

X_test <- X_test[[1]]
Y_test <- Y_test[[1]] %>%
  as.factor()

fitControl <- trainControl(method = 'repeatedcv',
                           number = 10,
                           repeats = 10)

svmfit <- train(X1, Y1,
                method = 'lssvmRadial',
                trControl = fitControl)
