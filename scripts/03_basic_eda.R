############################
### Name: Nate Rowan
### Project: Sports Info Solutions 2020 Challenge
### Date Created: 7/15/2020
### Date Last Modified: 7/15/2020
### Description: Exploratory Data Analysis
############################
library(tidyverse); theme_set(theme_minimal())
library(patchwork)


### Read in the data
data <- read_csv(here::here("datasets/raw_sis_data.csv"), na = "NULL")



# Where do linemen line up? -----------------------------------------------



### Make the Technique Name into a factor
data <- data %>%
  mutate(
    TechniqueName = as_factor(TechniqueName)
  )

### Change order of technique to the line order
data$TechniqueName <- fct_relevel(data$TechniqueName, "0", "1", "2i", "2", "3", "4i", "4", "5", "7", "6", "9", "Outside", "Off Ball")

### Create bar chart showing distribution of where D-Line line up on left side
left_side <- data %>%
  filter(SideOfBall == "L") %>%
  ggplot(aes(x = fct_rev(TechniqueName))) +
    geom_bar() +
    scale_y_continuous(limits = c(0, 11000)) +
    labs(
      title = "Distribution of where defensive linemen line up",
      x = "Left of Center"
    )

### Create bar chart showing guys who line up on the center
center <- data %>%
  filter(TechniqueName == "0") %>%
  ggplot(aes(x = TechniqueName)) +
    geom_bar() +
    scale_y_continuous(limits = c(0, 11000)) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y.left = element_blank(),
      aspect.ratio = 8/1
    ) 

### Create bar chart showing distribution of where D-Line line up on right side
right_side <- data %>%
  filter(SideOfBall == "R") %>%
  ggplot(aes(x = TechniqueName)) +
    geom_bar() +
    scale_y_continuous(limits = c(0, 11000)) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y.left = element_blank()
    ) +
    labs(
      x = "Right of Center"
    )

### Show entire distribution of where linemen line up
left_side + center + right_side







  
  
  
  
  
  
  
  
  
  












