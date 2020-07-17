############################
### Name: Nate Rowan
### Project: Sports Info Solutions 2020 Challenge
### Date Created: 7/17/2020
### Date Last Modified: 7/17/2020
### Description: Creating a metric to evaluate the value of d-linemen on passes
############################
library(tidyverse)

### Read in the data
data <- read_csv(here::here("datasets/raw_sis_data.csv"), na = "NULL")


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



# Creating the Passing EPA metric -----------------------------------------

### Pull out each play from the passes data (no duplicates based on player data)
pass_plays <- passes %>%
  group_by(GameID, EventID) %>%
  slice(1) %>%
  ungroup()

### Average EPA of any pass play
avg_pass_epa <- pass_plays %>%
  summarise(avg_epa = mean(EPA, na.rm = TRUE)) %>%
  as.numeric()

### Calculate the EPA value of Pressures, Sacks, and Forced Fumbles
pass_rush_epa_values_table <- pass_plays %>%
  group_by(PressureOnPlay, SackOnPlay, FumbleByPasser) %>%
  summarise(
    avg_epa = mean(EPA, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup() %>%
  filter(n > 5) %>%
  mutate(epa_diff = avg_epa - -.01786565) %>% ### -.01786565 is the average EPA on passes for this data
  arrange(desc(avg_epa)) %>%
  select(
    PressureOnPlay:avg_epa, epa_diff, n
  )

### Function that pulls out the EPA change for each possible combo of pressure, sack, and fumble
calculate_pass_rush_stat_value <- function (data, pressure, sack, fumble) {
  data %>%
    filter(PressureOnPlay == pressure, SackOnPlay == sack, FumbleByPasser == fumble) %>%
    pull(epa_diff)
}

### Pull the coefficient for each type of play
no_pressure_value <- calculate_pass_stat_value(pass_rush_epa_values_table, 0, 0, 0)
pressure_value <- calculate_pass_stat_value(pass_rush_epa_values_table, 1, 0, 0)
sack_value <- calculate_pass_stat_value(pass_rush_epa_values_table, 1, 1, 0)
fumble_value <- calculate_pass_stat_value(pass_rush_epa_values_table, 1, 1, 1)


### This guy includes pass breakups and interceptions. Focusing on passes defended
pass_defense_epa_values_table <- pass_plays %>%
  group_by(PassBreakupOnPlay, InterceptionOnPlay) %>%
  summarise(
    avg_epa = mean(EPA, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup() %>%
  filter(n > 5) %>%
  mutate(epa_diff = avg_epa - -.01786565) %>% ### -.01786565 is the average EPA on passes for this data
  arrange(desc(avg_epa)) %>%
  select(
    PassBreakupOnPlay:avg_epa, epa_diff, n
  )

### Function that pulls out the EPA change for each possible combo of pbu and int
calculate_pass_defense_stat_value <- function (data, pbu, int) {
  data %>%
    filter(PassBreakupOnPlay == pbu, InterceptionOnPlay == int) %>%
    pull(epa_diff)
}

### Pull the pbu and int coefficients 
pbu_value <- calculate_pass_defense_stat_value(pass_defense_epa_values_table, pbu = 1, int = 0)
int_value <- calculate_pass_defense_stat_value(pass_defense_epa_values_table, pbu = 1, int = 1)



### Example of Chandler Jones's numbers in the second half of 2019
passes %>%
  filter(Name == "Chandler Jones") %>%
  select(Pressure:ForcedFumble) %>%
  summarise_if(is.numeric, sum)