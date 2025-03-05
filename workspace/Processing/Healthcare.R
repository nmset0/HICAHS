library(tidyverse)
library(readr)
rm(list = ls())

health <- read_csv("C:/Users/natha/OneDrive/Documents/internship/workspace/PLACES__Local_Data_for_Better_Health__County_Data_2024_release_20250303.csv")
colnames(health) <- tolower(names(health))
state_names <- c("Colorado", "Montana", "North Dakota", "South Dakota", "Utah", "Wyoming")
health <- filter(health, state %in% state_names) |> arrange(stateabbr)

