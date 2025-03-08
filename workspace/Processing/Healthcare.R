
library(tidyverse)
library(readxl)

# # health risks
# health <- read_csv("~/internship/workspace/PLACES__Local_Data_for_Better_Health__County_Data_2024_release_20250306.csv")
# colnames(health) <- tolower(names(health))
# state_names <- c("Colorado", "Montana", "North Dakota", "South Dakota", "Utah", "Wyoming")
# health <- filter(health, state %in% state_names) |> arrange(stateabbr, county) |> select(-locationid)
#
# # health facilities
# colorado <- read_excel("~/internship/workspace/Health Facility Data/ColoradoHealth24_xlsx.xlsx", sheet = 2)
# wyoming <- read_excel("~/internship/workspace/Health Facility Data/WyomingHealth24_xlsx.xlsx", sheet = 2)
# south_dakota <- read_excel("~/internship/workspace/Health Facility Data/SouthDakotaHealth24_xlsx.xlsx", sheet = 2)
# north_dakota <- read_excel("~/internship/workspace/Health Facility Data/NorthDakotaHealth24_xlsx.xlsx")
# utah <- read_excel("~/internship/workspace/Health Facility Data/UtahHealth24_xlsx.xlsx", sheet = 2)
# montana <- read_excel("~/internship/workspace/Health Facility Data/MontanaHealth24_xlsx.xlsx")
#
# # H2-A population
# h2a_population <- read_csv("~/internship/workspace/Written Datasets/h2a_by_county_new.csv")

healthcare <- function() {
co_data <- subset(health, state == "Colorado")
co_data <- co_data |>
  left_join(colorado, by = "county") |>
  select(-state.y) |>
  rename(state = state.x) |>
  left_join(h2a_population, by = c("county", "state"))


wyoming$county <- gsub(" County", "", wyoming$county)
wy_data <- subset(health, state == "Wyoming") |>
  left_join(wyoming, by = "county") |>
  select(-state.y) |>
  rename(state = state.x) |>
  left_join(h2a_population, by = c("county", "state"))

sdkt_data <- subset(health, state == "South Dakota")
sdkt_data <- sdkt_data |>
  left_join(south_dakota, by = "county") |>
  select(-state.y) |>
  rename(state = state.x) |>
  left_join(h2a_population, by = c("county", "state"))

ndkt_data <- subset(health, state == "North Dakota")
ndkt_data <- ndkt_data |>
  left_join(north_dakota, by = "county") |>
  select(-state.y) |>
  rename(state = state.x) |>
  left_join(h2a_population, by = c("county", "state"))

ut_data <- subset(health, state == "Utah")
ut_data <- ut_data |>
  left_join(utah, by = "county") |>
  select(-state.y) |>
  rename(state = state.x) |>
  left_join(h2a_population, by = c("county", "state"))

montana$county <- str_to_title(montana$county)
mt_data <- subset(health, state == "Montana")
mt_data <- mt_data |>
  left_join(montana, by = "county") |>
  select(-state.y) |>
  rename(state = state.x) |>
  left_join(h2a_population, by = c("county", "state"))

assign("co_data", co_data, envir = .GlobalEnv)
assign("ut_data", ut_data, envir = .GlobalEnv)
assign("wy_data", wy_data, envir = .GlobalEnv)
assign("mt_data", mt_data, envir = .GlobalEnv)
assign("ndkt_data", ndkt_data, envir = .GlobalEnv)
assign("sdkt_data", sdkt_data, envir = .GlobalEnv)

}


