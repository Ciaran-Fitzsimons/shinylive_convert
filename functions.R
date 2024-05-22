library(dplyr)
library(scales)

### Function to create conversion rate with first time shots
calculate_conversion_rate_by_first_time <- function(shot_data, goal_data) {
  # Summarise shots by 'First time'
  shots_by_first_time <- shot_data %>%
    group_by(`First time`) %>%
    summarise(Shots = n())
  
  # Summarise goals by 'First time'
  goals_by_first_time <- goal_data %>%
    group_by(`First time`) %>%
    summarise(Goals = n())
  
  # Join the summarised data
  conversion_rate_by_first_time <- left_join(shots_by_first_time, goals_by_first_time, by = "First time")
  
  # Replace NA values with 0 and calculate Goals per shot
  conversion_rate_by_first_time <- conversion_rate_by_first_time %>%
    select(`First time`, Goals, Shots) %>%
    replace(is.na(.), 0) %>%
    mutate(`Conversion rate` = percent(Goals / Shots))
  
  return(conversion_rate_by_first_time)
}


### Function to create conversion rate by foot
calculate_conversion_rate_by_foot <- function(shot_data, goal_data) {
  # Summarise shots by 'Foot'
  shots_by_foot <- shot_data %>%
    group_by(Foot) %>%
    summarise(Shots = n())
  
  # Summarise goals by 'Foot'
  goals_by_foot <- goal_data %>%
    group_by(Foot) %>%
    summarise(Goals = n())  

  # Join the summarised data
  conversion_rate_by_foot <- left_join(shots_by_foot, goals_by_foot, by = "Foot")
  
  # Replace NA values with 0 and calculate Goals per shot
  conversion_rate_by_foot <- conversion_rate_by_foot %>%
    select(Foot, Goals, Shots) %>%
    replace(is.na(.), 0) %>%
    mutate(`Conversion rate` = percent(Goals / Shots))
  
  return(conversion_rate_by_foot)  
}


### Function to create conversion rate by type of shot
calculate_conversion_rate_by_type_of_shot <- function(shot_data, goal_data) {
  # Summarise shots by 'Foot'
  shots_by_type <- shot_data %>%
    group_by(Type) %>%
    summarise(Shots = n())
  
  # Summarise goals by 'Foot'
  goals_by_type <- goal_data %>%
    group_by(Type) %>%
    summarise(Goals = n())  
  
  # Join the summarised data
  conversion_rate_by_type_of_shot <- left_join(shots_by_type, goals_by_type, by = "Type")
  
  # Replace NA values with 0 and calculate Goals per shot
  conversion_rate_by_type_of_shot <- conversion_rate_by_type_of_shot %>%
    select(Type, Goals, Shots) %>%
    replace(is.na(.), 0) %>%
    mutate(`Conversion rate` = percent(Goals / Shots))
  
  return(conversion_rate_by_type_of_shot)  
}

### Function to create circles and arcs for the heatmap
circle_function_heatmap <- function(center=c(0,0), diameter=20, npoints=1000, start=0, end=2, rotation_angle = 0){
  tt <- seq(start*pi, end*pi, length.out=npoints)
  circle_data <- data.frame(
    x = center[1] + diameter / 2 * cos(tt),
    y = center[2] + diameter / 2 * sin(tt)
  )
  rotated_x <- circle_data$x * cos(rotation_angle) - circle_data$y * sin(rotation_angle)
  rotated_y <- circle_data$x * sin(rotation_angle) + circle_data$y * cos(rotation_angle)
  data.frame(
    x = rotated_x,
    y = rotated_y
  )
}
