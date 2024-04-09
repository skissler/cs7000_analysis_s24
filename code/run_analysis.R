# ==============================================================================
# Import
# ==============================================================================

# library(MASS)
library(mvtnorm)
library(tidyverse) 
library(rstan) 

source('code/utils.R') 

# Turn the datasets into tidy format: 
loblolly <- as_tibble(Loblolly) %>% 
	select(Seed, Age=age, Height=height)

iris <- as_tibble(iris) %>% 
	mutate(Flower=1:n()) %>% 
	pivot_longer(-c("Species","Flower"), names_to="Attribute", values_to="Value") %>% 
	separate_wider_delim(
		cols=Attribute, delim=".", names=c("Structure","Metric"))

ct_dat <- read_csv("data/ct_dat_sample.csv") %>% 
	select(InfectionIndex, TestDateIndex, AgeGrp, Ct)

# ==============================================================================
# Generate exploratory plots for each dataset
# ==============================================================================

source("code/loblolly_explore.R")

# source("code/iris_explore.R")

source("code/ct_dat_explore.R")

# ==============================================================================
# Do some Bayesian fits 
# ==============================================================================

source("code/loblolly_bayes.R")

# source("code/iris_bayes.R")













