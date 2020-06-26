library(tidyverse)

urlfile = "https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2020/master/Data/AnalyticsChallenge2020Data.csv"

data <- read_csv(url(urlfile))

write_csv(data, "datasets/raw_sis_data.csv")
