#### Model Data Preparation

## General outline of what needs to be done:

# - read in a sample as a dataframe
# - convert relevant variables to factor
# - split data frame by course into a list of data frames (OG List)
# * define formula you want to use and call model.matrix on every df in OG List
#   * this will create new list of estimation-ready matrices
# * split estimation-ready matrices into train/test
#     * create two lists, one for train and one for test


#------------------------------------------------------------------------------#


### 0. Library setup and import -----------------------------------------------#
libs <- c("tidyverse", "data.table", "caret")
lapply(libs, library, character.only = TRUE)

ivs <- c("acad_level", "age_at_class_start", "course_load",
         "ethnicity", "first_gen", "gender", "in_state",
         "modality", "netcost_wloan", "nth_term", "pell_eligibility",
         "prev_term_gpa", "school_year", "session_code", "term",
         "took_instructor", "took_course", "transfer")

charToFactor <- function(data, ignore_vars) {
  if (missing(ignore_vars)){
    char_vars <- which(sapply(data, is.character))
  } else {
    char_vars <- which(sapply(data, is.character))
    char_vars <- setdiff(char_vars, char_vars[ignore_vars])
  }
  
  data <- data %>%
    mutate_at(char_vars, factor)
  
  return(data)
}

intToFactor <- function(data, ignore_vars) {
  if (missing(ignore_vars)){
    ind_vars <- which(sapply(data, is.integer))
  } else {
    ind_vars <- which(sapply(data, is.integer))
    ind_vars <- setdiff(ind_vars, ind_vars[ignore_vars]) 
  }
  
  data <- data %>%
    mutate_at(ind_vars, factor)
  
  return(data)
}

data <- fread("Data/sample.csv", data.table = FALSE) %>%
  select(one_of(ivs), dv, course)

OG <- split(data, data$course) %>%
  lapply(charToFactor, c("course")) %>%
  lapply(intToFactor, c("age_at_class_start", "course_load")) %>%
  lapply(function(x) select(x, -course))


### 1. Manually set all reference levels --------------------------------------#
setReference <- function(data, variables, references) {
  ### Force reference level of factor to a specific level
  
  ## Have to throw this in a try block because there will be courses that
  ## do not have the specified reference level available
  for (i in seq_along(variables)){
    tryCatch({
      
      data[, variables[i]] <- relevel(data[, variables[i]], references[i])
    
    }, error = function(err){
      
    }, finally = {
      next
    })
  }
  return(data)
}

relevel_variables <- c('acad_level', 'ethnicity', 'first_gen', 'gender', 'in_state',
                  'modality', 'nth_term', 'pell_eligibility', 'school_year',
                  'session_code', 'term', 'took_instructor', 'took_course', 
                  'transfer')

references <- c('Freshman', 'W', '0', 'M', '0', 'ASUFACETOFACE', '1', 'N',
                  '2008', 'C', 'Fall', '0', '0', '0')

OG <- lapply(OG, setReference, relevel_variables, references)


### 2. Split and scale data ---------------------------------------------------#
splitScale <- function(data, ivs, dv){
  ### Create indicator for train/test split and scale by training set
  
  # Args:
  #   - data: data frame
  #   - ivs: character vector of columns used
  
  # Returns:
  #   - A single data frame with variable `group` and levels ('train', 'test')
  #     to indicate training/test set
  #     - numeric variables are scaled and centered
  
  ## set the seed to make your partition reproductible
  set.seed(1)
  
  Y <- data[, dv]
  
  train_index <- createDataPartition(Y, p = .8,
                                     list = FALSE,
                                     times = 1)
  
  X_train <- data[train_index, ] %>%
    select(one_of(ivs))
  
  X_test <- data[-train_index, ] %>%
    select(one_of(ivs))
  
  # Center and scale each to the training set
  scaling_values <- preProcess(X_train, method = c('center', 'scale'))
  
  X_train_scaled <- predict(scaling_values, X_train)
  X_test_scaled <- predict(scaling_values, X_test)
  
  # Pull out train/test dv vector according to index
  Y_train <- data.frame(dv = Y[train_index])
  Y_test <- data.frame(dv = Y[-train_index])
  
  # Center Y if gaussian
  if (family == 'gaussian') {
    Y_scaling_values <- preProcess(Y_train, method = 'center')
    
    Y_train <- predict(Y_scaling_values, Y_train)
    Y_test <- predict(Y_scaling_values, Y_test)
  }
  
  Y <- bind_rows(Y_train, Y_test)
  
  names(Y) <- dv
  
  # Recombine and create variable to tell the groups apart
  X_scaled <- bind_rows('train' = X_train_scaled, 'test' = X_test_scaled, .id = 'group')
  
  all_data <- cbind(X_scaled, Y)
  return(all_data)
}

data <- lapply(OG, splitScale, ivs, dv)

train <- lapply(data, function(x) filter(x, group == 'train')) %>%
  lapply(function(x) select(x, -group))

test <- lapply(data, function(x) filter(x, group == 'test')) %>%
  lapply(function(x) select(x, -group))


### 3. Create model matrices --------------------------------------------------#
formula <- as.formula(paste(dv, "~", paste(ivs, collapse =' + ')))

X_train <- lapply(train, function(x) model.matrix(formula, x)) %>%
  lapply(function(x) x[, -1])

X_test <- lapply(test, function(x) model.matrix(formula, x)) %>%
  lapply(function(x) x[, -1])

Y_train <- lapply(train, function(x) select(x, dv)) %>%
  lapply(as.matrix) 

Y_test <- lapply(test, function(x) select(x, dv)) %>%
  lapply(as.matrix)

rm(train, test, data)


### 4. Check variable integrity (IN PROGRESS) ---------------------------------#
dropBadVars <- function(data) {
  ### Check for near-zero variance and correlation of predictors
  ## Drop columns that are unfit for estimation
  
  # If a column is near-zero variance, we need to drop the column as well
  # as any row where the value of that column = 1.This is a blunt rule that only
  # accounts for shitty binary variables, but I don't expect us to have any wonky
  # continuous variables
  
  near_zero_indices <- nearZeroVar(data)
  high_corr_indices <- findCorrelation(cor(data))
  
  dropCols <- intersect(near_zero_indices, high_corr_indices) %>%
    sort(decreasing = TRUE)
  
  
  dropLowVarCol <- function(data){
    
    near_zero_indices <- nearZeroVar(data) %>%
      sort(decreasing = TRUE)
    
    for (nz_index in near_zero_indices) {
      data <- data[data[, nz_index] != 1, -nz_index]
    }
    
    return(data)
  }

  dropCorrCol <- function(data) {
    
    indices <- findCorrelation(cor(data)) %>%
      sort(decreasing = TRUE)
    
    for (index in indices){
      data <- data[data[, index] != 1, -index]
    }
    
    return(data)  
  }
  
  data <- dropLowVarCol(data) %>%
    dropCorrCol()

  return(data)
}


