#############################################
#### Interactive Advertising Project ####
# Script for Simulating Data (SEM)
# Created on Nov 10, 2025, by Zhanming Chen
# Checked on DATE, by NAME
#############################################

#### AI USE DISCLOSURE ####
# When searching for the codes I need, such as  
# how to use the lavaan package,
# Google automatically presents the AI-generated results that 
# includes sample codes and use cases. 
# I partially refer to the AI-generated results
# and check the actual search results to validate the codes. 
# The references were listed as comments. 

#### Packages installation (optional) ####

install.packages("faux") # for rnorm
install.packages("lavaan") # for sem
install.packages("summarytools") # for checking
install.packages("dplyr") # for checking
install.packages("missMethods") # for miss numbers 
install.packages("groundhog") # for packages

#### Package loading ####

library(faux)
library(lavaan)
library(summarytools)
library(dplyr)
library(missMethods)
library(groundhog)

#### Workspace setup ####

sessionInfo()

# R version 4.5.1
# faux_1.2.3
# lavaan_0.6-20
# summarytools_1.1.4
# dplyr_1.1.4
# groundhog_3.2.3     
# missMethods_0.4.0

groundhog.library(faux, "2025-11-10")
groundhog.library(lavaan, "2025-11-10")
groundhog.library(summarytools, "2025-11-10")
groundhog.library(dplyr, "2025-11-10")
groundhog.library(missMethods, "2025-11-10")

sessionInfo() # check again

# setting seed for reproducibility

set.seed(1997)

#############################################

# get info on data specs from dictionary

dict <- read.csv("./Data/interactive_ads_data_dictionary_20251110.csv")
dict

# create empty data frame and populate with id variable (N = 600)

N = 600
dat_sim <- data.frame(id = c(1:N))
head(dat_sim)

# [consent] simulate data for consent, all 1

dat_sim$consent_sim <- 1

# [group] simulate two groups: interactive ads (drag and drop) and non-interactive ads (flat posters)

## create 300 of 1 followed by 300 of 2
dat_sim$group_sim <- rep(c(1, 2), each = 300)

## not shuffle them for lavaan codes
# dat_sim$group_sim <- sample(group_sim_ini)
table(dat_sim$group_sim)

## Note the number of participants in group 1 and group 2 as parameters

n_group_1 <- sum(dat_sim$group_sim == 1)
n_group_2 <- sum(dat_sim$group_sim == 2)
n_group_1
n_group_2

# [focus_1, focus_2, focus_3] simulate data for focus questions

dat_sim$focus_1_sim <- 5
dat_sim$focus_2_sim <- 2
dat_sim$focus_3_sim <- 6

# set parameters for different probabilities

## left skewed and right skewed 7-point data

prob_right_skewed <- c(.03, .04, .05, .08, .10, .30, .40)
prob_left_skewed <- c(.40, .30, .10, .08, .05, .04, .03)

# create a function for the repetitive data generating process
## Documentation for a function in R: https://r-pkgs.org/man.html 

#' Generate data for two groups using sample function (The description did not work yet)
#'
#' This function generates two groups of data within a grouping variable. 
#'
#' @param range_1 An integer. The starting value of the range for the sample function.
#' @param range_2 An integer. The ending value of the range for the sample function.
#' @param group_var The column for group assignment.
#' @param group_val_1 The specific value within group_var that identifies the first group.
#' @param group_val_2 The specific value within group_var that identifies the second group.
#' @param prob_1 A list of probability for sampling in group 1. Right skewed.
#' @param prob_2 A list of probability for sampling in group 2. Left skewed. 
#'
#' @return A list of the simulated data. 
#' @examples
#' my_groups <- factor(rep(c("A", "B", "C"), times = 10))
#'
#' # Generate data for groups "A" and "B" from the range 1-5.
#' # Group "C" will remain 0.
#' sim_data <- data_generate_sample(
#'   range_1 = 1,
#'   range_2 = 5,
#'   group_var = my_groups,
#'   group_val_1 = "A",
#'   group_val_2 = "B",
#'   prob_1 = NULL,
#'   prob_2 = NULL
#' )
data_generate_sample <- function(range_1, range_2, group_var, group_val_1, group_val_2, prob_1, prob_2) {
  
  ### initialize an empty column for indexing later
  ### numeric(): https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/numeric 
  dat_sim <- numeric(length(group_var))
  
  ### calculate the size of group 1 and 2
  ### sum(): https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/sum 
  size_group_1 <- sum(group_var == group_val_1)
  size_group_2 <- sum(group_var == group_val_2)
  
  ### simulate data for group 1 and 2
  ### indexing: https://sudo-labs.github.io/r-data-science/vectors/
  dat_sim[group_var == group_val_1] <- sample(range_1:range_2, size = size_group_1, replace = TRUE, prob = prob_1)
  dat_sim[group_var == group_val_2] <- sample(range_1:range_2, size = size_group_2, replace = TRUE, prob = prob_2)
  
  return(dat_sim)
}

# simulate data for graphi

## [graphi_1] simulate data for two groups

### initialize blank 'grapics_x_sim' columns (necessary? -> no)
### numeric(): https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/numeric 
# dat_sim$graphi_1_sim <- numeric(length(dat_sim$group_sim))
# dat_sim$graphi_2_sim <- numeric(length(dat_sim$group_sim))
# dat_sim$graphi_3_sim <- numeric(length(dat_sim$group_sim))

### initially the codes were ran one variable by another, but the codes were too long.
### I created the data_generate_sample() to simplify
### Use a for loop to further simplify
n_graphi <- 3
for (i in 1:n_graphi) {
  col_title <- paste0("graphi_", i, "_sim") # paste() didn't work
  dat_sim[[col_title]] <- data_generate_sample(
    range_1 = 1,
    range_2 = 7,
    group_var = dat_sim$group_sim,
    group_val_1 = 1,
    group_val_2 = 2,
    prob_1 = prob_right_skewed,
    prob_2 = prob_left_skewed
  )
}

### a quick check
### descr(): https://www.rdocumentation.org/packages/summarytools/versions/1.0.1/topics/summarytools-package
dat_sim %>% dplyr::select(graphi_1_sim:graphi_3_sim) %>% summarytools::descr()

###########################

# simulate data for six variables in the SEM using lavaan: inte, soci, symp, play, effi, dona
# References
## (Sturman, 2025) "Model-Based Approach to Data Simulation in R"
## https://lavaan.ugent.be/tutorial/
## https://psu-psychology.github.io/r-bootcamp-2018/talks/lavaan_tutorial.html#:~:text=1%20Introduction,the%20model%20specification%20in%20detail 
## https://quantdev.ssri.psu.edu/tutorials/structural-equation-modeling-r-using-lavaan

## set up lavaan model

model_syntax <- '
  # measurement model: assign items for each of the variables
  
  ## IV: Interactivity
  
  INTE_SIM =~ 0.8*inte_1_sim + 0.7*inte_2_sim + 0.8*inte_3_sim + 0.7*inte_4_sim + 0.8*inte_5_sim
  
  ## Mediator 1: Social Presence
  
  SOCI_SIM =~ 0.8*soci_1_sim + 0.7*soci_2_sim + 0.8*soci_3_sim + 0.7*soci_4_sim + 0.8*soci_5_sim
  
  ## Mediator 2: Sympathy
  
  SYMP_SIM =~ 0.8*symp_1_sim + 0.7*symp_2_sim + 0.8*symp_3_sim + 0.7*symp_4_sim + 0.8*symp_5_sim + 0.7*symp_6_sim + 0.8*symp_7_sim + 0.7*symp_8_sim + 0.8*symp_9_sim + 0.8*symp_10_sim

  ## Mediator 3: Perceived Playfulness
  
  PLAY_SIM =~ 0.8*play_1_sim + 0.7*play_2_sim + 0.8*play_3_sim + 0.7*play_4_sim

  ## Mediator 4: Perceived Response Efficacy

  EFFI_SIM =~ 0.8*effi_1_sim + 0.7*effi_2_sim + 0.8*effi_3_sim + 0.7*effi_4_sim
  
  ## DV: Donation Intention

  DONA_SIM =~ 0.8*dona_1_sim + 0.7*dona_2_sim + 0.8*dona_3_sim

  # regression
  
  ## IV -> Mediators 1, 2, 3
  ## strength of regression
  
  SOCI_SIM ~ 0.4*INTE_SIM
  SYMP_SIM ~ 0.3*INTE_SIM
  PLAY_SIM ~ 0.5*INTE_SIM
  
  ## Mediators 1, 2, 3 -> Mediator 4
  
  EFFI_SIM ~ 0.4*SOCI_SIM + 0.3*SYMP_SIM + 0.5*PLAY_SIM
  
  ## Mediator 4 -> DV
  
  DONA_SIM ~ 0.4*SOCI_SIM + 0.3*SYMP_SIM + 0.5*PLAY_SIM + 0.5*EFFI_SIM

  # residual correlations
  
  ## fix variance to 1
  
  INTE_SIM ~~ 1*INTE_SIM
  SOCI_SIM ~~ 1*SOCI_SIM
  SYMP_SIM ~~ 1*SYMP_SIM
  PLAY_SIM ~~ 1*PLAY_SIM
  EFFI_SIM ~~ 1*EFFI_SIM
  DONA_SIM ~~ 1*DONA_SIM
  
  ## add correlations between mediators
  
  SOCI_SIM ~~ 0.2*SYMP_SIM
  SOCI_SIM ~~ 0.2*PLAY_SIM
  SYMP_SIM ~~ 0.2*PLAY_SIM

  # set latent means to distinguish two groups

  # fix Group 2 (lower interactivity) to a mean of 0, and fix Group 1 (higher interactivity) to a mean of 0.8.

  INTE_SIM ~ c(0.8, 0)*1
'

## generate simulated but continous data (not 1-7) in two groups using lavaan

sim_data_continuous <- simulateData(model_syntax, sample.nobs = c(n_group_1, n_group_2))

## convert continuous data to 1-7

item_names <- setdiff(names(sim_data_continuous), "group")
sim_data_int <- sim_data_continuous %>%
  mutate( # modifies columns
    across(all_of(item_names), 
           ~cut(., # cuts continuous data into seven categories
                breaks = quantile(., probs = seq(0, 1, length.out = 8), na.rm = TRUE), 
                labels = 1:7, 
                include.lowest = TRUE),
           .names = "{.col}") # use the original column's name
  ) %>%
  mutate(across(all_of(item_names), ~as.numeric(as.character(.))))

## add these new data into dataframe, dat_sim

# cbind() to column-bind 
dat_sim <- cbind(dat_sim, sim_data_int[item_names])
head(dat_sim)
tail(dat_sim)

# simulate data for demographics

## [age] simulate age data using rnorm

# set.seed(1997)
dat_sim$age_sim <- round(rnorm(n = 600, mean = 39, sd = 8)) # round(): age should be int
head(dat_sim$age_sim)
mean(dat_sim$age_sim)
sd(dat_sim$age_sim)

## [gender] simulate gender data using sample()

# set.seed(1997)
dat_sim$gender_sim <- sample(1:4, size = 600, replace = TRUE, prob = c(.30, .30, .30, .10))
table(dat_sim$gender_sim)

## [race] simulate race data using sample()

# set.seed(1997)
dat_sim$race_sim <- sample(1:7, size = 600, replace = TRUE, prob = c(.16, .16, .16, .16, .16, .16, .04))
table(dat_sim$race_sim)

## [income] simulate income data using sample()

# set.seed(1997)
dat_sim$income_sim <- sample(1:13, size = 600, replace = TRUE, prob = c(.08, .08, .08, .08, .08, .08, .08, .08, .08, .08, .08, .08, .04))
table(dat_sim$income_sim)

## [edu] simulate education data using sample()

# set.seed(1997)
dat_sim$edu_sim <- sample(1:8, size = 600, replace = TRUE, prob = c(.14, .14, .14, .14, .14, .14, .14, .02))
table(dat_sim$edu_sim)

# do a few checks to make sure all looks right

dim(dat_sim)
head(dat_sim)
tail(dat_sim)

# all fields will be mandatory and there will be no missing data in the results (Qualtrics)

# check

dat_sim %>% dplyr::select(focus_1_sim:edu_sim) %>% summarytools::freq()
dat_sim

# save the new file out and use for preregistration

write.csv(dat_sim, "./Data/interactive_ads_data_simulated_lavaan_20251112.csv", row.names = FALSE)

