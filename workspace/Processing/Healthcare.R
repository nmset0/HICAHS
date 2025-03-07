rm(list = ls())

library(tidyverse)
library(readxl)

# health risks
health <- read_csv("workspace/PLACES__Local_Data_for_Better_Health__County_Data_2024_release_20250306.csv")
colnames(health) <- tolower(names(health))
state_names <- c("Colorado", "Montana", "North Dakota", "South Dakota", "Utah", "Wyoming")
health <- filter(health, state %in% state_names) |> arrange(stateabbr, county)

# health facilities
colorado <- read_excel("workspace/Health Facility Data/ColoradoHealth24_xlsx.xlsx", sheet = 2)
wyoming <- read_excel("workspace/Health Facility Data/WyomingHealth24_xlsx.xlsx", sheet = 2)
south_dakota <- read_excel("workspace/Health Facility Data/SouthDakotaHealth24_xlsx.xlsx", sheet = 2)
north_dakota <- read_excel("workspace/Health Facility Data/NorthDakotaHealth24_xlsx.xlsx")
utah <- read_excel("workspace/Health Facility Data/UtahHealth24_xlsx.xlsx", sheet = 2)
montana <- read_excel("workspace/Health Facility Data/MontanaHealth24_xlsx.xlsx")


# data merging
# List of all datasets
datasets <- list(utah, wyoming, montana, colorado, south_dakota, north_dakota)

# Find common column names
common_columns <- Reduce(intersect, lapply(datasets, colnames))

# Display common columns
print(common_columns)
