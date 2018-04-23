### Data Setup
### Author: William Morgan

## Purpose:
# Subset the full peoplesoft data set to extract relevant columns 
# and observations

## List of drops:
# no post-baccs, law students, or graduate students
# students taking more than 24 credits
# students with unspecified gender
# courses with "ASU" as its subject
# courses worth more than 4 units (attempting to capture internships here)
# "Dynamic" session courses
# irregular grades (only want A-E + W)
# courses that are not lectures
# students past their 16th observed term
# missing residency values
# incoming_gpa > 4.33
# missing college values
# irregular campus locations
# graduate courses

## New Columns not already in PS data
# course_level factor variable for UD, LD
# truncated CIP code called "acad_field"

#------------------------------------------------------------------------------#

## 0. Setup
rm(list = ls())

libs <- c("tidyverse", "data.table")
lapply(libs, library, character.only = TRUE)

# PS rootpath
ps_path <-  "R:\\Data\\ASU Core\\Student Data\\Peoplesoft Data\\"

#------------------------------------------------------------------------------#

## 1. Importing and selecting columns
cols <- c("strm", "session_code", "acad_plan", "emplid", "unt_taken",
          "passing_m", "course_load", "course", "took_course",
          "descr", "mmap_course", "transfer", "crse_grade_off", "residency",
          "ethnicity", "gender", "acad_level", "ssr_component", "subject",
          "location", "age_at_class_start", "catalog_nbr", "school_year",
          "core_course", "ncore_taken_bot", "nth_term", "term", "incoming_gpa",
          "complete", "corbetter", "mastery", "prev_term_gpa", "cgrade_wdrw", 
          "cgrade", "modal", "class_nbr", "first_gen", "pell_eligibility",
          "netcost_wloan", "part_time_unof", "took_instructor", "college",
          "instructor_id", "part_time_offi")

data <- fread(paste0(ps_path, "full_data.csv"),
              select = cols)



#------------------------------------------------------------------------------#

## 2. Drops

data <- data %>%
  filter(!acad_level %in% c("Graduate", "Law", "Post Bacc Graduate", "Post-Bacc Graduate"),
         course_load <= 24,
         gender != "U",
         subject != "ASU",
         unt_taken <= 4,
         session_code != "DYN",
         crse_grade_off %in% c("A", "A-", "A+", "B", "B+", "B-",
                               "C", "C+", "C-", "D", "E", "W"),
         ssr_component == "LEC",
         nth_term <= 16,
         residency != "",
         incoming_gpa <= 4.33,
         !is.na(college),
         location %in% c("ASUONLINE", "DTPHX", "ICOURSE", "ONLINE", "POLY",
                         "TEMPE", "WEST"),
         catalog_nbr <= 499) %>%
  mutate(pell_eligibility = if_else(pell_eligibility == "", "U", pell_eligibility),
         term = if_else(term == "Winter", "Fall", term),
         course_level = if_else(catalog_nbr >= 300, "upper division", "lower division"),
         id = paste(emplid, strm, class_nbr, sep = "_")) %>%
  select(-c(class_nbr, acad_plan, catalog_nbr, subject, descr,
            instructor_id, ssr_component, unt_taken)) %>%
  rename(grade = crse_grade_off,
            campus = location,
            modality = modal,
            part_time = part_time_unof) %>%
  select(sort(current_vars()))

#------------------------------------------------------------------------------#

## 3. Export

fwrite(data, "Data/ps_data.csv")

small_sample <- sample_frac(data, .1)
fwrite(small_sample, "Data/ps_subset.csv")
