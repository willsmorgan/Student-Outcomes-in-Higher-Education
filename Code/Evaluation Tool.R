#### Model Evaluation

## Binomial Metrics:
# Misclassification rate
# AUC
# Sensitivity
# Specificity

## Regression Metrics:
# RMSE

#------------------------------------------------------------------------------#
libs <- c('pROC')
lapply(libs, library, character.only = TRUE)

modelEval <- function(model) {
  
  ## Run AUC, Misclass rate, Sens/Spec if classification
  if (family == 'binomial') {
    
    # Train Values
    train_responses <- Y_train %>%
      map(as.vector, mode = 'numeric')
    
    # Test Values
    test_responses <- Y_test %>%
      map(as.vector, mode = 'numeric')
    
    # Predicted training labels
    p_train_labels <- model[['train_predictions']] %>%
      map(round)
    
    # Predicted testing labels
    p_test_labels <- model[['test_predictions']] %>%
      map(round)
    
    # Training set results
    train_results <- pmap(list(predicted_prob = model[['train_predictions']],
                               predicted_label = p_train_labels,
                               actual_label = train_responses),
                          data.frame)
    
    # Test set results
    test_results <- pmap(list(predicted_prob = model[['test_predictions']],
                              predicted_label = p_test_labels,
                              actual_label = test_responses),
                         data.frame)
    
    ## AUC
    train_auc <- map2(train_responses, model[['train_predictions']], roc) %>%
      map(auc, ci = TRUE) %>%
      map(1) %>%
      map(data.frame) %>%
      bind_rows(.id = 'course') %>%
      rename(auc = `.x..i..`)
    
    test_auc <- map2(test_responses, model[['test_predictions']], roc) %>%
      map(auc, ci = TRUE) %>%
      map(1) %>%
      map(data.frame) %>%
      bind_rows(.id = 'course') %>%
      rename(auc = `.x..i..`)
    
    auc <- bind_rows("train" = train_auc,
                     "test" = test_auc,
                     .id = "set")
    
    rm(train_auc, test_auc)
    
    ## Misclassification rate
    train_miss_rate <- train_results %>%
      map(function(x) mutate(x, incorrect = if_else(predicted_label != actual_label, 1, 0))) %>%
      map(function(x) summarise(x, misclass_rate = mean(incorrect))) %>%
      map(as.numeric) %>%
      map(data.frame) %>%
      bind_rows(.id = 'course') %>%
      rename(misclass_rate = `.x..i..`)
    
    test_miss_rate <- test_results %>%
      map(function(x) mutate(x, incorrect = if_else(predicted_label != actual_label, 1, 0))) %>%
      map(function(x) summarise(x, misclass_rate = mean(incorrect))) %>%
      map(as.numeric) %>%
      map(data.frame) %>%
      bind_rows(.id = 'course') %>%
      rename(misclass_rate = `.x..i..`)
    
    miss_rate <- bind_rows("train" = train_miss_rate,
                           "test" = test_miss_rate, 
                           .id = "set")
    
    rm(train_miss_rate, test_miss_rate)
    
    # Sensitivity/Specificity Calculation
    train_sens <- train_results %>%
      map(function(x) filter(x, actual_label == 1)) %>%
      map(function(x) mutate(x, correct = if_else(predicted_label == actual_label, 1, 0))) %>%
      map(function(x) summarise(x, sensitivity = mean(correct))) %>%
      map(as.numeric) %>%
      map(data.frame) %>%
      bind_rows(.id = 'course') %>%
      rename(sensitivity = `.x..i..`)
    
    test_sens <- test_results %>%
      map(function(x) filter(x, actual_label == 1)) %>%
      map(function(x) mutate(x, correct = if_else(predicted_label == actual_label, 1, 0))) %>%
      map(function(x) summarise(x, sensitivity = mean(correct))) %>%
      map(as.numeric) %>%
      map(data.frame) %>%
      bind_rows(.id = 'course') %>%
      rename(sensitivity = `.x..i..`)
    
    train_spec <- train_results %>%
      map(function(x) filter(x, actual_label == 1)) %>%
      map(function(x) mutate(x, correct = if_else(predicted_label == actual_label, 1, 0))) %>%
      map(function(x) summarise(x, specificity = mean(correct))) %>%
      map(as.numeric) %>%
      map(data.frame) %>%
      bind_rows(.id = 'course') %>%
      rename(specificity = `.x..i..`)
    
    test_spec <- test_results %>%
      map(function(x) filter(x, actual_label == 1)) %>%
      map(function(x) mutate(x, correct = if_else(predicted_label == actual_label, 1, 0))) %>%
      map(function(x) summarise(x, sensitivity = mean(correct))) %>%
      map(as.numeric) %>%
      map(data.frame) %>%
      bind_rows(.id = 'course') %>%
      rename(specificity = `.x..i..`)
    
    sens <- bind_rows("train" = train_sens,
                      "test" = test_sens,
                      .id = 'set')
    
    spec <- bind_rows("train" = train_spec,
                      "test" = test_spec,
                      .id = 'set')
    
    sens_spec <- inner_join(sens, spec, by = 'course')
    rm(train_sens, test_sens, train_spec, test_spec, sens, spec)
    
    # List all results for output
    eval_metrics <- list('misclassification_rate' = miss_rate,
                         'auc' = auc,
                         'sens_spec' = sens_spec)
    
    
    ## Run RMSE if regression
  } else if (family == 'gaussian') {
    
    train_responses <- Y_train
    
    test_responses <- Y_test
    
    # Calculate RMSE on test set
    calcRMSE <- function(actual, prediction) {
      rmse <- sqrt(mean((actual - prediction)^2))
      return(rmse)
    }
    
    train_rmse <- map2(train_responses, model[['train_predictions']], calcRMSE) %>%
      map(data.frame) %>%
      bind_rows(.id = 'course') %>%
      rename(rmse = `.x..i..`)
    
    test_rmse <- map2(test_responses, model[['test_predictions']], calcRMSE) %>%
      map(data.frame) %>%
      bind_rows(.id = 'course') %>%
      rename(rmse = `.x..i..`)
    
    rmse <- bind_rows("train" = train_rmse,
                      "test" = test_rmse,
                      .id = 'set')
    
    eval_metrics <- list("rmse" = rmse)
  }
  
  return(eval_metrics)
}