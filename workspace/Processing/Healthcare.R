library(tidyverse)
library(readxl)

# health risks
health <- read_csv("~/internship/workspace/PLACES__Local_Data_for_Better_Health__County_Data_2024_release_20250306.csv")
colnames(health) <- tolower(names(health))
state_names <- c("Colorado", "Montana", "North Dakota", "South Dakota", "Utah", "Wyoming")
health <- filter(health, state %in% state_names) |> arrange(stateabbr, county) |> select(-locationid)

# health facilities
colorado <- read_excel("~/internship/workspace/Health Facility Data/ColoradoHealth24_xlsx.xlsx", sheet = 2)
wyoming <- read_excel("~/internship/workspace/Health Facility Data/WyomingHealth24_xlsx.xlsx", sheet = 2)
south_dakota <- read_excel("~/internship/workspace/Health Facility Data/SouthDakotaHealth24_xlsx.xlsx", sheet = 2)
north_dakota <- read_excel("~/internship/workspace/Health Facility Data/NorthDakotaHealth24_xlsx.xlsx")
utah <- read_excel("~/internship/workspace/Health Facility Data/UtahHealth24_xlsx.xlsx", sheet = 2)
montana <- read_excel("~/internship/workspace/Health Facility Data/MontanaHealth24_xlsx.xlsx")

# H2-A population
h2a_population <- read_csv("~/internship/workspace/Written Datasets/h2a_by_county_new.csv") |> arrange(state, county)

# Function for Healthcare.rmd
healthcare <- function() {
  # Join health factors with H2-A populations for each state
  # (+ some data modification when needed)
  co_data <- subset(health, state == "Colorado")
  co_data <- co_data |>
    left_join(colorado, by = "county") |>
    select(-state.y) |>
    rename(state = state.x) |> # Remove duplicate state columns
    left_join(h2a_population, by = c("county", "state")) # Joining H2-A populations, avoiding mismatching
                                                         # with states that have same county names

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

  # Add to global environment
  assign("co_data", co_data, envir = .GlobalEnv)
  assign("ut_data", ut_data, envir = .GlobalEnv)
  assign("wy_data", wy_data, envir = .GlobalEnv)
  assign("mt_data", mt_data, envir = .GlobalEnv)
  assign("ndkt_data", ndkt_data, envir = .GlobalEnv)
  assign("sdkt_data", sdkt_data, envir = .GlobalEnv)
}
healthcare()

stateFacilityCorrelations <- function() {
# Initialize all 6 data frames for storing correlation coefficients
co_corr <-
  mt_corr <-
  ndkt_corr <-
  sdkt_corr <-
  wy_corr <-
  ut_corr <- data.frame(
    Predictor = character(),
    Correlation = numeric(),
    PValue = numeric(),
    Significance = numeric())


# Colorado correlations: migrant population vs quantities of given health facilities
co_predictors <- names(co_data[, (which(names(co_data) == "geolocation") + 1):(which(names(co_data) == "total_workers_h2a") - 1)])
for (c in co_predictors) {
  cor_test <- cor.test(co_data[[c]], co_data$total_workers_h2a, method = "spearman", exact = F, use = "pairwise.complete.obs")
  co_corr <- rbind(co_corr, data.frame(
    Predictor = c,
    Correlation = round(cor_test$estimate, 3),
    PValue = cor_test$p.value))
}
co_corr <- co_corr |>
  mutate(Variable = "total_workers_h2a", .before = Predictor) |>
  mutate(State = "Colorado", .before = Variable)
co_corr$Significance = ifelse(co_corr$PValue <= 0.05, TRUE, FALSE)


# Montana correlations: migrant population vs quantities of given health facilities
mt_predictors <- names(mt_data[, (which(names(mt_data) == "geolocation") + 1):(which(names(mt_data) == "total_workers_h2a") - 1)])
for (m in mt_predictors) {
  cor_test <- cor.test(mt_data[[m]], mt_data$total_workers_h2a, method = "spearman", exact = F, use = "pairwise.complete.obs")
  mt_corr <- rbind(mt_corr, data.frame(
    Predictor = m,
    Correlation = round(cor_test$estimate, 3),
    PValue = cor_test$p.value))
}
mt_corr <- mt_corr |>
  mutate(Variable = "total_workers_h2a", .before = Predictor) |>
  mutate(State = "montana", .before = Variable)
mt_corr$Significance = ifelse(mt_corr$PValue <= 0.05, TRUE, FALSE)


# North Dakota correlations: migrant population vs quantities of given health facilities
ndkt_predictors <- names(ndkt_data[, (which(names(ndkt_data) == "geolocation") + 1):(which(names(ndkt_data) == "total_workers_h2a") - 1)])
for (n in ndkt_predictors) {
  cor_test <- cor.test(ndkt_data[[n]], ndkt_data$total_workers_h2a, method = "spearman", exact = F, use = "pairwise.complete.obs")
  ndkt_corr <- rbind(ndkt_corr, data.frame(
    Predictor = n,
    Correlation = round(cor_test$estimate, 3),
    PValue = cor_test$p.value))
}
ndkt_corr <- ndkt_corr |>
  mutate(Variable = "total_workers_h2a", .before = Predictor) |>
  mutate(State = "North Dakota", .before = Variable)
ndkt_corr$Significance = ifelse(ndkt_corr$PValue <= 0.05, TRUE, FALSE)


# South Dakota correlations: migrant population vs quantities of given health facilities
sdkt_predictors <- names(sdkt_data[, (which(names(sdkt_data) == "geolocation") + 1):(which(names(sdkt_data) == "total_workers_h2a") - 1)])
for (s in sdkt_predictors) {
  cor_test <- cor.test(sdkt_data[[s]], sdkt_data$total_workers_h2a, method = "spearman", exact = F, use = "pairwise.complete.obs")
  sdkt_corr <- rbind(sdkt_corr, data.frame(
    Predictor = s,
    Correlation = round(cor_test$estimate, 3),
    PValue = cor_test$p.value))
}
sdkt_corr <- sdkt_corr |>
  mutate(Variable = "total_workers_h2a", .before = Predictor) |>
  mutate(State = "South Dakota", .before = Variable)
sdkt_corr$Significance = ifelse(sdkt_corr$PValue <= 0.05, TRUE, FALSE)


# Wyoming correlations: migrant population vs quantities of given health facilities
wy_predictors <- names(wy_data[, (which(names(wy_data) == "geolocation") + 1):(which(names(wy_data) == "total_workers_h2a") - 1)])
for (Y in wy_predictors) {
  cor_test <- cor.test(wy_data[[Y]], wy_data$total_workers_h2a, method = "spearman", exact = F, use = "pairwise.complete.obs")
  wy_corr <- rbind(wy_corr, data.frame(
    Predictor = Y,
    Correlation = round(cor_test$estimate, 3),
    PValue = cor_test$p.value))
}
wy_corr <- wy_corr |>
  mutate(Variable = "total_workers_h2a", .before = Predictor) |>
  mutate(State = "Wyoming", .before = Variable)
wy_corr$Significance = ifelse(wy_corr$PValue <= 0.05, TRUE, FALSE)


# Utah correlations: migrant population vs quantities of given health facilities
ut_predictors <- names(ut_data[, (which(names(ut_data) == "geolocation") + 1):(which(names(ut_data) == "total_workers_h2a") - 1)])
for (u in ut_predictors) {
  cor_test <- cor.test(ut_data[[u]], ut_data$total_workers_h2a, method = "spearman", exact = F, use = "pairwise.complete.obs")
  ut_corr <- rbind(ut_corr, data.frame(
    Predictor = u,
    Correlation = round(cor_test$estimate, 3),
    PValue = cor_test$p.value))
}
ut_corr <- ut_corr |>
  mutate(Variable = "total_workers_h2a", .before = Predictor) |>
  mutate(State = "Utah", .before = Variable)
ut_corr$Significance = ifelse(ut_corr$PValue <= 0.05, TRUE, FALSE)

# binding the data together
h2aPopulationCorrelations <- rbind(co_corr, wy_corr, ndkt_corr, sdkt_corr, ut_corr) |> dplyr::arrange(State, Correlation)
assign("h2aPopulationCorrelations", h2aPopulationCorrelations, envir = globalenv())
}

stateFacilityCorrelations()


# Total H2-A workers per county & health conditions reported:
# co_data$total_workers_h2a[is.na(co_data$total_workers_h2a)] <- 0
# df <- co_data |> select(state, county, short_question_text, total_workers_h2a)
#
# # Pivot the data
# pivoted_df <- df %>%
#   group_by(county) %>%
#   pivot_wider(names_from = short_question_text, values_from = total_workers_h2a, values_fn = sum)
#
# # View the resulting data frame
# pivoted_df |> left_join(h2a_population |> filter(state == "Colorado"), by = "county") |> select(-state.y) |> view()

