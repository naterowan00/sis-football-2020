############################
### Name: Nate Rowan
### Project: Sports Info Solutions 2020 Challenge
### Date Created: 6/25/2020
### Date Last Modified: 6/25/2020
### Description: Clean up the SIS dataset
############################

### Load in required packages
library(tidyverse)
library(janitor)
library(here) # used in filename when reading in the data file

### Read in the SIS data set
raw_sis_data <- read_csv(here("datasets/raw_sis_data.csv"))

### Change column names to lower_snake_case
clean_data <- raw_sis_data %>%
  clean_names()








