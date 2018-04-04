#### Sample Selection

## Purpose:
# Encode categorical variables
# Final cleaning
# Define the sample for a given enrollment total criteria

#------------------------------------------------------------------------------#

## 0: Setup

rm(list = ls())
libs <- c("tidyverse", "data.table")
lapply(libs, library, character.only = TRUE)

data <-  fread("Data/ps_subset.csv") %>%
  select(-c(num_credits, campus, core_taken, mmap_taken, acad_field, college)) %>%

# data <-  fread("Data/ps_data.csv") %>%
#   select(-c(num_credits, campus, core_taken, mmap_taken, acad_field, college)) %>%

#------------------------------------------------------------------------------#

## 1. Inspecting variables

# num_credits can be dropped 
# campus can be dropped

# prev_term_gpa has some 0 values but are not all freshmen
# core_taken and mmap_taken should probably be proportions
# *_taken measures student's broader schedule and *_course is directly the course
# acad_field way too broad and should only be included in larger models
# college won't have enough variation and should only include in larger models

##### IDEA:
### plot enrollment filter against number of courses that fit criteria

#### ANOTHER IDEA:
### vary enrollment size criteria and restimate to find point at which the most
### amount of courses are included and the error overall is the smallest

#------------------------------------------------------------------------------#

## 2. Final cleaning for sample selection
# Final criteria for inclusion:
#   - undergraduate courses
#   - after 2012
#   - more than 100 students enrolled in each modality

# Grab course names that fit criteria
course_count <- data %>%
  filter(course_level != 'graduate') %>%
  group_by(course, modality) %>%
  tally() %>%
  spread(modality, n) %>%
  replace_na(list(ASUFACETOFACE = 0, ASUONLINE = 0, ICOURSE = 0)) %>%
  mutate(total_enroll = ASUONLINE + ASUFACETOFACE + ICOURSE) %>%
  filter(total_enroll >= 3000) %>%
  mutate(f2f_ind = if_else(ASUFACETOFACE <= 500, 1, 0),
         ico_ind = if_else(ICOURSE <= 500, 1, 0),
         asuo_ind = if_else(ASUONLINE <= 500, 1, 0),
         drop = f2f_ind + ico_ind + asuo_ind) %>%
  filter(drop < 2)

fwrite(course_count, 'Data/course_counts.csv')

# Merge course names to retrieve sample
courses <- course_count$course
sample <- data %>%
  filter(course %in% courses)

# Save sample
fwrite(sample, 'Data/sample.csv')

#------------------------------------------------------------------------------#