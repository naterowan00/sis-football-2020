############################
### Name: Nate Rowan
### Project: Sports Info Solutions 2020 Challenge
### Date Created: 6/25/2020
### Date Last Modified: 6/25/2020
### Description: Read in the SIS file from GitHub and save it
###              as a .csv file locally
############################

### Load in required packages
library(tidyverse) 

### URL with the data from the SIS contest GitHub site
urlfile = "https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv"

### Read in the data from the SIS GitHub site
data <- read_csv(url(urlfile))

### Save the file as a .csv locally
write_csv(data, "datasets/raw_sis_data.csv")
