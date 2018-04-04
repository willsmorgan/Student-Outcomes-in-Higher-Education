#### Model Estimation and Evaluation

# Load models and evaluation tool
# Run a model and evaluate results for a given DV

#------------------------------------------------------------------------------#

### Section 1: Setup
rm(list = ls(all = TRUE))

# Load model definitions and evaluation function
source("Code/Model Definitions.R")
source("Code/Evaluation Tool.R")

#------------------------------------------------------------------------------#

### Section 2: Estimation

dv <- 'aorbetter'
family <- 'binomial'

source("Code/Model Prep.R", local = TRUE)

model <- Enet()
