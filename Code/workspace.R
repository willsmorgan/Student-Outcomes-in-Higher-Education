#### Workspace
## This script is meant for testing code before it is put into the main script

#------------------------------------------------------------------------------#

###### Plot number of courses included in sample against varying levels of criteria
data <-  fread("Data/ps_data.csv") %>%
  select(-c(num_credits, campus)) %>%
  filter(session_code != "DYN")

criteria <- c(10, 25, 50, 75, 100, 150, 200, 250, 300, 400, 500, 750, 1000)
num_courses <- vector(mode = 'integer', length(criteria))

for (c in seq_along(criteria)){
  num_courses[c] <- data %>%
    filter(school_year >= 2012) %>%
    filter(course_level != "graduate") %>%
    filter(modality != "ICOURSE") %>%
    group_by(course, modality) %>%
    tally() %>%
    spread(modality, n) %>%
    filter_at(c("ASUFACETOFACE", "ASUONLINE"), all_vars(. >= criteria[c])) %>%
    mutate(f2f_o_ratio = ASUFACETOFACE / ASUONLINE) %>%
    nrow()
}

data.frame(criteria, num_courses) %>%
  ggplot(aes(criteria, num_courses)) +
  geom_line()

#------------------------------------------------------------------------------#

##### Getting distributions on DVs
dv_summaries <- data %>%
  mutate(count = 1) %>%
  group_by(course) %>%
  summarise_at(c("count", "aorbetter", "borbetter", "passing_m"), sum) %>%
  mutate(prob_a = aorbetter / count,
         prob_b = borbetter / count,
         prob_pass = passing_m / count) %>%
  select(course, count, prob_a, prob_b, prob_pass)

dv_summaries %>%
  filter(count > 100) %>%
  ggplot(aes(prob_pass)) +
  geom_histogram(binwidth = .05)

#------------------------------------------------------------------------------#

#### Data frame of course-level model results (accuracy, sens, spec)
errors <- map2(all_models, X_test, predict, s = 'lambda.min', type = 'class') %>%
  map2(., Y_test, cbind) %>%
  map(as.data.frame, stringsAsFactors = FALSE) %>%
  bind_rows(.id = 'course') %>%
  rename(label = `1`) %>%
  mutate(correct = if_else(passing_m == label, 1, 0),
         true_positive = if_else(passing_m == 1 & label == 1, 1, 0),
         true_negative = if_else(passing_m == 0 & label == 0, 1, 0),
         false_positive = if_else(passing_m == 0 & label == 1, 1, 0),
         false_negative = if_else(passing_m == 1 & label == 0, 1, 0),
         label = as.numeric(label),
         passing_m = as.numeric(passing_m)) %>%
  group_by(course) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  mutate(accuracy = correct / count,
         sensitivity = true_positive / (true_positive + false_negative),
         specificity = true_negative / (true_negative + false_positive)) %>%
  select(course, count, label, accuracy, everything()) %>%
  rename(num_predicted_successes = label,
         num_actual_successes = passing_m,
         n_obs = count)

#------------------------------------------------------------------------------#

