# Student-Outcomes-in-Higher-Education
Model building to predict various measures of undergraduate student success

## Purpose

Our goal is to test a handful of machine learning techniques in predicting outcomes in college courses. Data is gathered using the host university's Student Information System (SIS) and accounts for a wide array of student and course characteristics. Cleaning is (mostly) done in Stata 14, while the rest of the project is written in R.

The project proceeds in two directions - we first split our sample into course-level data sets and for a given model type and measure, estimate a model for each course. This allows us to extract effects unique to individual courses, at the cost of a smaller sample. The second portion of this project treats all courses in the sample as one, and estimates a model for the entire data set. This analysis is intended to be complementary to the initial course-level models, as it gives us more informed descriptions of the macro environment. 


## Completed scripts

- Course-level sample selection
- Data preparation for modeling (Model Prep.R)
- Model Evaluation tool

## In-progress scripts

- Model definitions
- Model estimation and evaluation
- Population-level sample selection
