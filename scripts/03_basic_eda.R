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




# Which linemen get the most pressure? ------------------------------------

### Pull out all pass attempts from the data
passes <- data %>%
  filter(EventType == "pass")

### There was pressure on 36.1% of dropbacks
passes %>%
  summarise(pressures = mean(PressureOnPlay))


### Here is the distribution of pressures by roster position, with pressure rate included
roster_pressure <- passes %>%
  group_by(RosterPosition) %>%
  summarise(
    rushes = sum(IsRushing),
    pressures = sum(Pressure),
  ) %>%
  mutate(
    pressure_rate = pressures / rushes
  )

### Plot showing pressure success rate graphically. Secondary most successful, then LB,
### then DE, then DT
roster_pressure %>%
  ggplot(aes(x = RosterPosition, y = pressure_rate)) +
    geom_col()

### Here is the distribution of pressures by on field postion, with pressure rate included
field_pos_pressure <- passes %>%
  group_by(OnFieldPosition) %>%
  summarise(
    rushes = sum(IsRushing),
    pressures = sum(Pressure),
  ) %>%
  mutate(
    pressure_rate = pressures / rushes
  )

### Plot showing pressure rate graphically. Linebackers succeed 12% of time, linemen 8%
field_pos_pressure %>%
  ggplot(aes(x = OnFieldPosition, y = pressure_rate)) +
    geom_col()

### Here is the distribution of pressures by technique, with pressure rate included
tech_pressure <- passes %>%
  group_by(TechniqueName) %>%
  summarise(
    rushes = sum(IsRushing),
    pressures = sum(Pressure),
  ) %>%
  mutate(
    pressure_rate = pressures / rushes
  )

### Plot showing pressure rate graphically. Interesting takeaway is that the further
### outside you get, the more success there is at getting pressure on the QB
tech_pressure %>%
  ggplot(aes(x = TechniqueName, y = pressure_rate)) +
    geom_col()


### This plot shows the frequency at which each technique rushes the QB.
### Should be kept in mind with the previous success rate plot
tech_pressure %>%
  ggplot(aes(x = TechniqueName, y = rushes)) +
    geom_col()




### Next thing to explore: How does pressure affect the EPA of a play?
### Then, I might be able to quantify the value of a pressure. Perhaps after
### that I could quantify the value of a sack, interception, etc

### Pull out each play from the passes data (no duplicates based on player data)
pass_plays <- passes %>%
  group_by(GameID, EventID) %>%
  slice(1) %>%
  ungroup()

### YASSS QUEEN this table shows average EPA of plays where there is pressure, a sack
### a QB fumble, and/or an interception. This is so cool. 
### MIGHT WANT TO REMOVE INTERCEPTION CONDITION
pass_plays %>%
  group_by(PressureOnPlay, SackOnPlay, FumbleByPasser) %>%
  summarise(
    avg_epa = mean(EPA, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup() %>%
  filter(n > 5) %>%
  arrange(desc(avg_epa))


### Average EPA of any pass play
pass_plays %>%
  summarise(avg_epa = mean(EPA, na.rm = TRUE))







# Rushing Analysis --------------------------------------------------------


  
  
  
  
  
  
  
  
  
  











