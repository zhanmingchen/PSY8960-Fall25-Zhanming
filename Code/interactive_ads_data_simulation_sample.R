#############################################
#### Interactive Advertising Project ####
# Script for Simulating Data (Sample Functions)
# Created on Nov 10, 2025, by Zhanming Chen
# Checked on DATE, by NAME
#############################################

#### Packages installation (optional) ####

install.packages("faux") # for rnorm
install.packages("summarytools") # for checking
install.packages("dplyr") # for checking
install.packages("missMethods") # for miss numbers 
install.packages("groundhog") # for packages

#### Package loading ####

library(faux)
library(summarytools)
library(dplyr)
library(missMethods)
library(groundhog)

#### Workspace setup ####

sessionInfo()

# R version 4.5.1
# faux_1.2.3
# summarytools_1.1.4
# dplyr_1.1.4
# groundhog_3.2.3     
# missMethods_0.4.0

groundhog.library(faux, "2025-11-10")
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

# simulate data for graphi

## [graphi_1] simulate data for two groups

### initialize a blank 'grapics_1_sim' column
dat_sim$graphi_1_sim <- 0

### select rows simulate for Group 1 (right-skewed)
dat_sim$graphi_1_sim[dat_sim$group_sim == 1] <- sample(1:7, size = n_group_1, replace = TRUE, prob = prob_right_skewed)

### select rows and simulate for Group 2 (left-skewed)
dat_sim$graphi_1_sim[dat_sim$group_sim == 2] <- sample(1:7, size = n_group_2, replace = TRUE, prob = prob_left_skewed)

### quick checks: number of each 7 points, mean and sd in each group
table(dat_sim$graphi_1_sim)

## [graphi_2] simulate data for two groups

### initialize a blank 'grapics_2_sim' column
dat_sim$graphi_2_sim <- 0

### select rows simulate for Group 1 (right-skewed)
dat_sim$graphi_2_sim[dat_sim$group_sim == 1] <- sample(1:7, size = n_group_1, replace = TRUE, prob = prob_right_skewed)

### select rows and simulate for Group 2 (left-skewed)
dat_sim$graphi_2_sim[dat_sim$group_sim == 2] <- sample(1:7, size = n_group_2, replace = TRUE, prob = prob_left_skewed)

### quick checks: number of each 7 points, mean and sd in each group
table(dat_sim$graphi_2_sim)

## [graphi_3] simulate data for two groups

### initialize a blank 'grapics_3_sim' column
dat_sim$graphi_3_sim <- 0

### select rows simulate for Group 1 (right-skewed)
dat_sim$graphi_3_sim[dat_sim$group_sim == 1] <- sample(1:7, size = n_group_1, replace = TRUE, prob = prob_right_skewed)

### select rows and simulate for Group 2 (left-skewed)
dat_sim$graphi_3_sim[dat_sim$group_sim == 2] <- sample(1:7, size = n_group_2, replace = TRUE, prob = prob_left_skewed)

### quick checks: number of each 7 points, mean and sd in each group
table(dat_sim$graphi_3_sim)

# simulate data for six variables: inte, soci, symp, play, effi, dona

## create a function for the repetitive process

data_generate_sample <- function(range_1, range_2, group_var, group_val_1, group_val_2, prob_1, prob_2) {
  
  ### Initialize an empty numeric vector
  sim_var <- numeric(length(group_var))
  
  ### calculate the size of group 1 and 2 inside the function
  size_1 <- sum(group_var == group_val_1)
  size_2 <- sum(group_var == group_val_2)
  
  ### simulate for group 1
  sim_var[group_var == group_val_1] <- sample(range_1:range_2, size = size_1, replace = TRUE, prob = prob_1)
  
  ### simulate for group 2
  sim_var[group_var == group_val_2] <- sample(range_1:range_2, size = size_2, replace = TRUE, prob = prob_2)
  
  return(sim_var)
}

## simulate data for inte_1 to inte_5, use replicate() and return a data frame

### initially the codes were ran one variable by another, but the codes were too long. I searched for the replicate() function to simplify this process

n_inte <- 5
inte_sim_all <- replicate(
  n = n_inte,  
  data_generate_sample(
    range_1 = 1, 
    range_2 = 7, 
    group_var = dat_sim$group_sim, 
    group_val_1 = 1, 
    group_val_2 = 2, 
    prob_1 = prob_right_skewed, 
    prob_2 = prob_left_skewed
  ),
  simplify = "data.frame"
)

### rename the new columns use paste0()
colnames(inte_sim_all) <- paste0("inte_", 1:n_inte, "_sim")

### cbind(): add new data frame at the end
dat_sim <- cbind(dat_sim, inte_sim_all)

### a quick check
dat_sim %>% dplyr::select(inte_1_sim:inte_5_sim) %>% summarytools::descr()

## simulate data for soci

n_soci <- 5
soci_sim_all <- replicate(
  n = n_soci,  
  data_generate_sample(
    range_1 = 1, 
    range_2 = 7, 
    group_var = dat_sim$group_sim, 
    group_val_1 = 1, 
    group_val_2 = 2, 
    prob_1 = prob_right_skewed, 
    prob_2 = prob_left_skewed
  ),
  simplify = "data.frame"
)

colnames(soci_sim_all) <- paste0("soci_", 1:n_soci, "_sim")

dat_sim <- cbind(dat_sim, soci_sim_all)

### a quick check
dat_sim %>% dplyr::select(soci_1_sim:soci_5_sim) %>% summarytools::descr()

## simulate data for symp

n_symp <- 10
symp_sim_all <- replicate(
  n = n_symp,  
  data_generate_sample(
    range_1 = 1, 
    range_2 = 7, 
    group_var = dat_sim$group_sim, 
    group_val_1 = 1, 
    group_val_2 = 2, 
    prob_1 = prob_right_skewed, 
    prob_2 = prob_left_skewed
  ),
  simplify = "data.frame"
)

colnames(symp_sim_all) <- paste0("symp_", 1:n_symp, "_sim")

dat_sim <- cbind(dat_sim, symp_sim_all)

### a quick check
dat_sim %>% dplyr::select(symp_1_sim:symp_10_sim) %>% summarytools::descr()

## simulate data for play

n_play <- 4
play_sim_all <- replicate(
  n = n_play,  
  data_generate_sample(
    range_1 = 1, 
    range_2 = 7, 
    group_var = dat_sim$group_sim, 
    group_val_1 = 1, 
    group_val_2 = 2, 
    prob_1 = prob_right_skewed, 
    prob_2 = prob_left_skewed
  ),
  simplify = "data.frame"
)

colnames(play_sim_all) <- paste0("play_", 1:n_play, "_sim")

dat_sim <- cbind(dat_sim, play_sim_all)

### a quick check
dat_sim %>% dplyr::select(play_1_sim:play_4_sim) %>% summarytools::descr()

## simulate data for effi

n_effi <- 4
effi_sim_all <- replicate(
  n = n_effi,  
  data_generate_sample(
    range_1 = 1, 
    range_2 = 7, 
    group_var = dat_sim$group_sim, 
    group_val_1 = 1, 
    group_val_2 = 2, 
    prob_1 = prob_right_skewed, 
    prob_2 = prob_left_skewed
  ),
  simplify = "data.frame"
)

colnames(effi_sim_all) <- paste0("effi_", 1:n_effi, "_sim")

dat_sim <- cbind(dat_sim, effi_sim_all)

### a quick check
dat_sim %>% dplyr::select(effi_1_sim:effi_4_sim) %>% summarytools::descr()

## simulate data for dona

n_dona <- 3
dona_sim_all <- replicate(
  n = n_dona,  
  data_generate_sample(
    range_1 = 1, 
    range_2 = 7, 
    group_var = dat_sim$group_sim, 
    group_val_1 = 1, 
    group_val_2 = 2, 
    prob_1 = prob_right_skewed, 
    prob_2 = prob_left_skewed
  ),
  simplify = "data.frame"
)

colnames(dona_sim_all) <- paste0("dona_", 1:n_dona, "_sim")

dat_sim <- cbind(dat_sim, dona_sim_all)

### a quick check
dat_sim %>% dplyr::select(dona_1_sim:dona_3_sim) %>% summarytools::descr()

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

write.csv(dat_sim, "./Data/interactive_ads_data_simulated_sample_20251112.csv", row.names = FALSE)




