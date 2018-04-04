#### Model Definitions

# Penalized Regression
# Support Vector Classification
#
#

#------------------------------------------------------------------------------#
libs <- c('doParallel', 'glmnet', 'e1071')
lapply(libs, library, character.only = TRUE)

## Penalized regression
Enet <- function(alpha = 1){
  
  ### Run cv penalized regression on a list of course models for a given  
  ### dependent variable
  
  ## Args:
  # alpha --> L1/L2 ratio; numeric
  
  ## Returns:
  # List with three elements: coefficients, test predictions, and train predictions
  # The length of each list will be the number of courses that are modelled
  
  # define sequence of lambda to use 
  lambda = exp(seq(log(1), log(.00001), length.out = 100))
  
  # define cluster
  cl <- makeCluster(10)
  registerDoParallel(cl)
  
  # Estimate all models
  print('Estimating...')
  models <- map2(X_train, Y_train, cv.glmnet, family = family,
                 standardize = FALSE, parallel = TRUE, alpha = alpha,
                 lambda = lambda)
  
  stopCluster(cl)
  rm(cl)
  
  # extract coefficients 
  coefs <- map(models, coef, s = 'lambda.min')
  
  
  # test set predictions
  test_predictions <- map2(models, X_test,
                           predict,
                           s = 'lambda.min',
                           type = 'response') %>%
    map(as.vector, mode = 'numeric')
  
  # training predictions
  train_predictions <- map2(models, X_train,
                            predict,
                            s = 'lambda.min',
                            type = 'response') %>%
    map(as.vector, mode = 'numeric')
  
  # List all results for output
  results <- list('coefficients' = coefs,
                  'test_predictions' = test_predictions,
                  'train_predictions' = train_predictions)
  
  return(results)
  
}

## Support Vector Machines
kernel = 'radial'

supportVec <- function(kernel = 'radial'){
  
  ### Estimate a cross-validated support vector machine model tuning for
  ### kernels, cost, and kernel hyperparameters
  
  ## Args:
  # ??
  
  ## Returns:
  # At the very least, test predictions and train predictions (IN PROGRESS)
  
  toFactor <- function(object) {
    # Convert an object to a factor if not already
    if(!inherits(object, "factor")) {
      tryCatch(
        {
          vec <- as.factor(object)
        },
        error = function(e) {
          message("Something went wrong :\\")
          message("Here's the original warning message:")
          message(e)
        },
        warning = function(w) {
          message('Converting the object resulted in a warning:')
          message(w)
        }
      )
    }
  }
  
  Y_train <- map(Y_train, toFactor)
  Y_test <- map(Y_test, toFactor)
  
  # Define parameter grid conditional on kernel type
  if (kernel == 'radial') {
    
    grid <- list(cost = exp(seq(log(.01), log(1000), length.out = 3)),
                 gamma = exp(seq(log(1e-5), log(1e3), length.out = 3)))
    
  } else if (kernel == 'linear') {
    
    grid <- list(cost = exp(seq(log(.01), log(1000), length.out = 100)))
    
  }
  
  print("Estimating...")
  
  models <- map2(X_train, Y_train, tune.svm,
                 ranges = grid,
                 scale = FALSE)
  
  # define cluster
  cl <- makeCluster(10)
  registerDoParallel(cl)
  
  model <- tune(svm, X_train[[1]], Y_train[[1]], ranges = grid, scale = FALSE)
  
  stopCluster(cl)
  rm(cl)
  
  
  
  
  
  
  
  
}