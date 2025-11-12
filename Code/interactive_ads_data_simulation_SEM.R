#############################################
#### Interactive Advertising Project ####
# Script for Simulating Data (SEM)
# Created on Nov 10, 2025, by Zhanming Chen
# Checked on DATE, by NAME
#############################################

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

# [graphi_1] simulate data for two groups

## initialize a blank 'grapics_1_sim' column
dat_sim$graphi_1_sim <- NA

## select rows simulate for Group 1 (right-skewed)
dat_sim$graphi_1_sim[dat_sim$group_sim == 1] <- sample(1:7, size = n_group_1, replace = TRUE, prob = prob_right_skewed)

## select rows and simulate for Group 2 (left-skewed)
dat_sim$graphi_1_sim[dat_sim$group_sim == 2] <- sample(1:7, size = n_group_2, replace = TRUE, prob = prob_left_skewed)

## quick checks: number of each 7 points, mean and sd in each group
table(dat_sim$graphi_1_sim)
mean(dat_sim$graphi_1_sim[dat_sim$group_sim == 1])
sd(dat_sim$graphi_1_sim[dat_sim$group_sim == 1])
mean(dat_sim$graphi_1_sim[dat_sim$group_sim == 2])
sd(dat_sim$graphi_1_sim[dat_sim$group_sim == 2])

# [graphi_2] simulate data for two groups

## initialize a blank 'grapics_2_sim' column
dat_sim$graphi_2_sim <- NA

## select rows simulate for Group 1 (right-skewed)
dat_sim$graphi_2_sim[dat_sim$group_sim == 1] <- sample(1:7, size = n_group_1, replace = TRUE, prob = prob_right_skewed)

## select rows and simulate for Group 2 (left-skewed)
dat_sim$graphi_2_sim[dat_sim$group_sim == 2] <- sample(1:7, size = n_group_2, replace = TRUE, prob = prob_left_skewed)

## quick checks: number of each 7 points, mean and sd in each group
table(dat_sim$graphi_2_sim)
mean(dat_sim$graphi_2_sim[dat_sim$group_sim == 1])
sd(dat_sim$graphi_2_sim[dat_sim$group_sim == 1])
mean(dat_sim$graphi_2_sim[dat_sim$group_sim == 2])
sd(dat_sim$graphi_2_sim[dat_sim$group_sim == 2])

# [graphi_3] simulate data for two groups

## initialize a blank 'grapics_3_sim' column
dat_sim$graphi_3_sim <- NA
## select rows simulate for Group 1 (right-skewed)
dat_sim$graphi_3_sim[dat_sim$group_sim == 1] <- sample(1:7, size = n_group_1, replace = TRUE, prob = prob_right_skewed)
## select rows and simulate for Group 2 (left-skewed)
dat_sim$graphi_3_sim[dat_sim$group_sim == 2] <- sample(1:7, size = n_group_2, replace = TRUE, prob = prob_left_skewed)
## quick checks: number of each 7 points, mean and sd in each group
table(dat_sim$graphi_3_sim)
mean(dat_sim$graphi_3_sim[dat_sim$group_sim == 1])
sd(dat_sim$graphi_3_sim[dat_sim$group_sim == 1])
mean(dat_sim$graphi_3_sim[dat_sim$group_sim == 2])
sd(dat_sim$graphi_3_sim[dat_sim$group_sim == 2])

# simulate data for six variables in the SEM using lavaan: inte, soci, symp, play, effi, dona
# References
## (Sturman, 2025) "Model-Based Approach to Data Simulation in R"
## https://lavaan.ugent.be/tutorial/
## https://quantdev.ssri.psu.edu/tutorials/structural-equation-modeling-r-using-lavaan)

## set up lavaan model

model_syntax <- '
  # measurement model: assign items for each of the variables
  
  ## IV: Interactivity
  ## 1-0.7^2=0.51
  
  INTE_SIM =~ 0.7*inte_1_sim + 0.7*inte_2_sim + 0.7*inte_3_sim + 0.7*inte_4_sim + 0.7*inte_5_sim
    inte_1_sim ~~ 0.51*inte_1_sim 
    inte_2_sim ~~ 0.51*inte_2_sim
    inte_3_sim ~~ 0.51*inte_3_sim
    inte_4_sim ~~ 0.51*inte_4_sim
    inte_5_sim ~~ 0.51*inte_5_sim
  
  ## Mediator 1: Social Presence
  
  SOCI_SIM =~ 0.7*soci_1_sim + 0.7*soci_2_sim + 0.7*soci_3_sim + 0.7*soci_4_sim + 0.7*soci_5_sim
    soci_1_sim ~~ 0.51*soci_1_sim
    soci_2_sim ~~ 0.51*soci_2_sim
    soci_3_sim ~~ 0.51*soci_3_sim
    soci_4_sim ~~ 0.51*soci_4_sim
    soci_5_sim ~~ 0.51*soci_5_sim
  
  ## Mediator 2: Sympathy
  
  SYMP_SIM =~ 0.7*symp_1_sim + 0.7*symp_2_sim + 0.7*symp_3_sim + 0.7*symp_4_sim + 0.7*symp_5_sim + 0.7*symp_6_sim + 0.7*symp_7_sim + 0.7*symp_8_sim + 0.7*symp_9_sim + 0.7*symp_10_sim
    symp_1_sim ~~ 0.51*symp_1_sim
    symp_2_sim ~~ 0.51*symp_2_sim
    symp_3_sim ~~ 0.51*symp_3_sim
    symp_4_sim ~~ 0.51*symp_4_sim
    symp_5_sim ~~ 0.51*symp_5_sim
    symp_6_sim ~~ 0.51*symp_6_sim
    symp_7_sim ~~ 0.51*symp_7_sim
    symp_8_sim ~~ 0.51*symp_8_sim
    symp_9_sim ~~ 0.51*symp_9_sim
    symp_10_sim ~~ 0.51*symp_10_sim

  ## Mediator 3: Perceived Playfulness
  
  PLAY_SIM =~ 0.7*play_1_sim + 0.7*play_2_sim + 0.7*play_3_sim + 0.7*play_4_sim
    play_1_sim ~~ 0.51*play_1_sim
    play_2_sim ~~ 0.51*play_2_sim
    play_3_sim ~~ 0.51*play_3_sim
    play_4_sim ~~ 0.51*play_4_sim

  ## Mediator 4: Perceived Response Efficacy

  EFFI_SIM =~ 0.7*effi_1_sim + 0.7*effi_2_sim + 0.7*effi_3_sim + 0.7*effi_4_sim
    effi_1_sim ~~ 0.51*effi_1_sim
    effi_2_sim ~~ 0.51*effi_2_sim
    effi_3_sim ~~ 0.51*effi_3_sim
    effi_4_sim ~~ 0.51*effi_4_sim

  ## DV: Donation Intention

  DONA_SIM =~ 0.7*dona_1_sim + 0.7*dona_2_sim + 0.7*dona_3_sim
    dona_1_sim ~~ 0.51*dona_1_sim
    dona_2_sim ~~ 0.51*dona_2_sim
    dona_3_sim ~~ 0.51*dona_3_sim

  # regression
  
  ## IV -> Mediators 1, 2, 3
  ## strength of regression: 0.5
  
  SOCI_SIM ~ 0.5*INTE_SIM
  SYMP_SIM ~ 0.5*INTE_SIM
  PLAY_SIM ~ 0.5*INTE_SIM
  
  ## Mediators 1, 2, 3 -> Mediator 4
  
  EFFI_SIM ~ 0.5*SOCI_SIM + 0.5*SYMP_SIM + 0.5*PLAY_SIM
  
  ## Mediator 4 -> DV
  
  DONA_SIM ~ 0.5*EFFI_SIM

  # residual correlations
  
  ## fix variance of the exogenous IV to 1
  
  INTE_SIM ~~ 1*INTE_SIM
  
  ## fix residual variances for latent variables
  
  SOCI_SIM ~~ 0.85*SOCI_SIM
  SYMP_SIM ~~ 0.85*SYMP_SIM
  PLAY_SIM ~~ 0.85*PLAY_SIM
  EFFI_SIM ~~ 0.85*EFFI_SIM
  DONA_SIM ~~ 0.85*DONA_SIM
  
  ## add correlations between mediators
  
  SOCI_SIM ~~ 0.2*SYMP_SIM
  SOCI_SIM ~~ 0.2*PLAY_SIM
  SYMP_SIM ~~ 0.2*PLAY_SIM

  # latent means to distinguish two groups

  # fix Group 2 (lower interactivity) to a mean of 0, and fix Group 1 (higher interactivity) to a mean of 0.8.

  INTE_SIM ~ c(0.8, 0)*1

  # fix all other latent means to 0 for both groups so that it will follow the rules set in regression part
  SOCI_SIM ~ c(0, 0)*1
  SYMP_SIM ~ c(0, 0)*1
  PLAY_SIM ~ c(0, 0)*1
  EFFI_SIM ~ c(0, 0)*1
  DONA_SIM ~ c(0, 0)*1
  
  # fix all item intercepts to 0 (default assumption)
  inte_1_sim ~ 0*1; inte_2_sim ~ 0*1; inte_3_sim ~ 0*1; inte_4_sim ~ 0*1; inte_5_sim ~ 0*1;
  soci_1_sim ~ 0*1; soci_2_sim ~ 0*1; soci_3_sim ~ 0*1; soci_4_sim ~ 0*1; soci_5_sim ~ 0*1;
  symp_1_sim ~ 0*1; symp_2_sim ~ 0*1; symp_3_sim ~ 0*1; symp_4_sim ~ 0*1; symp_5_sim ~ 0*1;
  symp_6_sim ~ 0*1; symp_7_sim ~ 0*1; symp_8_sim ~ 0*1; symp_9_sim ~ 0*1; symp_10_sim ~ 0*1;
  play_1_sim ~ 0*1; play_2_sim ~ 0*1; play_3_sim ~ 0*1; play_4_sim ~ 0*1;
  effi_1_sim ~ 0*1; effi_2_sim ~ 0*1; effi_3_sim ~ 0*1; effi_4_sim ~ 0*1;
  dona_1_sim ~ 0*1; dona_2_sim ~ 0*1; dona_3_sim ~ 0*1;
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

# save the new file out and use for preregistration! 

write.csv(dat_sim, "./Data/interactive_ads_data_simulated_lavaan_20251112.csv", row.names = FALSE)

