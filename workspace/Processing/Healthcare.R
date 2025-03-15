library(tidyverse)
library(readxl)
# Everything is a function for use in .rmd files
# (Should have done this before)

state_facility_load <- function() {
  # health risks
  health <- read_csv("~/internship/workspace/PLACES__Local_Data_for_Better_Health__County_Data_2024_release_20250306.csv")
  colnames(health) <- tolower(names(health))
  states <- c("Colorado", "Montana", "North Dakota", "South Dakota", "Utah", "Wyoming")
  health <- filter(health, state %in% states) |> arrange(stateabbr, county) |> select(-locationid)

  # health facilities
  colorado <- read_excel("~/internship/workspace/Health Facility Data/ColoradoHealth24_xlsx.xlsx", sheet = 2)
  wyoming <- read_excel("~/internship/workspace/Health Facility Data/WyomingHealth24_xlsx.xlsx", sheet = 2)
  wyoming$county <- gsub(" County", "", wyoming$county)
  south_dakota <- read_excel("~/internship/workspace/Health Facility Data/SouthDakotaHealth24_xlsx.xlsx", sheet = 2)
  north_dakota <- read_excel("~/internship/workspace/Health Facility Data/NorthDakotaHealth24_xlsx.xlsx")
  utah <- read_excel("~/internship/workspace/Health Facility Data/UtahHealth24_xlsx.xlsx", sheet = 2)
  montana <- read_excel("~/internship/workspace/Health Facility Data/MontanaHealth24_xlsx.xlsx")

  # H2-A population
  h2a_population <- read_csv("~/internship/workspace/Written Datasets/h2a_by_county_new.csv") |> arrange(state, county)

  assign("health", health, envir = .GlobalEnv)
  assign("colorado", colorado, envir = .GlobalEnv)
  assign("wyoming", wyoming, envir = .GlobalEnv)
  assign("montana", montana, envir = .GlobalEnv)
  assign("north_dakota", north_dakota, envir = .GlobalEnv)
  assign("south_dakota", south_dakota, envir = .GlobalEnv)
  assign("utah", utah, envir = .GlobalEnv)
  assign("h2a_population", h2a_population, envir = .GlobalEnv)
  assign("states", states, envir = .GlobalEnv)
}

state_facility_load()

# Function for Healthcare.rmd
healthcare <- function() {
  # Join health factors with H2-A populations for each state
  # (+ some data modification when needed)
  co_health <- subset(health, state == "Colorado")
  co_health <- co_health |>
    left_join(colorado, by = "county") |>
    select(-state.y) |>
    rename(state = state.x) |> # Remove duplicate state columns
    left_join(h2a_population, by = c("county", "state")) # Joining H2-A populations, avoiding mismatching
  # with states that have same county names

  wy_health <- subset(health, state == "Wyoming") |>
    left_join(wyoming, by = "county") |>
    select(-state.y) |>
    rename(state = state.x) |>
    left_join(h2a_population, by = c("county", "state"))

  sdkt_health <- subset(health, state == "South Dakota")
  sdkt_health <- sdkt_health |>
    left_join(south_dakota, by = "county") |>
    select(-state.y) |>
    rename(state = state.x) |>
    left_join(h2a_population, by = c("county", "state"))

  ndkt_health <- subset(health, state == "North Dakota")
  ndkt_health <- ndkt_health |>
    left_join(north_dakota, by = "county") |>
    select(-state.y) |>
    rename(state = state.x) |>
    left_join(h2a_population, by = c("county", "state"))

  ut_health <- subset(health, state == "Utah")
  ut_health <- ut_health |>
    left_join(utah, by = "county") |>
    select(-state.y) |>
    rename(state = state.x) |>
    left_join(h2a_population, by = c("county", "state"))

  montana$county <- str_to_title(montana$county)
  mt_health <- subset(health, state == "Montana")
  mt_health <- mt_health |>
    left_join(montana, by = "county") |>
    select(-state.y) |>
    rename(state = state.x) |>
    left_join(h2a_population, by = c("county", "state"))

  # Add to global environment
  assign("co_health", co_health, envir = .GlobalEnv)
  assign("ut_health", ut_health, envir = .GlobalEnv)
  assign("wy_health", wy_health, envir = .GlobalEnv)
  assign("mt_health", mt_health, envir = .GlobalEnv)
  assign("ndkt_health", ndkt_health, envir = .GlobalEnv)
  assign("sdkt_health", sdkt_health, envir = .GlobalEnv)
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
  co_predictors <- names(co_health[, (which(names(co_health) == "geolocation") + 1):(which(names(co_health) == "total_workers_h2a") - 1)])
  for (c in co_predictors) {
    cor_test <- cor.test(co_health[[c]], co_health$total_workers_h2a, method = "spearman", exact = F, use = "pairwise.complete.obs")
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
  mt_predictors <- names(mt_health[, (which(names(mt_health) == "geolocation") + 1):(which(names(mt_health) == "total_workers_h2a") - 1)])
  for (m in mt_predictors) {
    cor_test <- cor.test(mt_health[[m]], mt_health$total_workers_h2a, method = "spearman", exact = F, use = "pairwise.complete.obs")
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
  ndkt_predictors <- names(ndkt_health[, (which(names(ndkt_health) == "geolocation") + 1):(which(names(ndkt_health) == "total_workers_h2a") - 1)])
  for (n in ndkt_predictors) {
    cor_test <- cor.test(ndkt_health[[n]], ndkt_health$total_workers_h2a, method = "spearman", exact = F, use = "pairwise.complete.obs")
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
  sdkt_predictors <- names(sdkt_health[, (which(names(sdkt_health) == "geolocation") + 1):(which(names(sdkt_health) == "total_workers_h2a") - 1)])
  for (s in sdkt_predictors) {
    cor_test <- cor.test(sdkt_health[[s]], sdkt_health$total_workers_h2a, method = "spearman", exact = F, use = "pairwise.complete.obs")
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
  wy_predictors <- names(wy_health[, (which(names(wy_health) == "geolocation") + 1):(which(names(wy_health) == "total_workers_h2a") - 1)])
  for (Y in wy_predictors) {
    cor_test <- cor.test(wy_health[[Y]], wy_health$total_workers_h2a, method = "spearman", exact = F, use = "pairwise.complete.obs")
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
  ut_predictors <- names(ut_health[, (which(names(ut_health) == "geolocation") + 1):(which(names(ut_health) == "total_workers_h2a") - 1)])
  for (u in ut_predictors) {
    cor_test <- cor.test(ut_health[[u]], ut_health$total_workers_h2a, method = "spearman", exact = F, use = "pairwise.complete.obs")
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
# co_health$total_workers_h2a[is.na(co_health$total_workers_h2a)] <- 0
# df <- co_health |> select(state, county, short_question_text, total_workers_h2a)
#
# # Pivot the data
# pivoted_df <- df %>%
#   group_by(county) %>%
#   pivot_wider(names_from = short_question_text, values_from = total_workers_h2a, values_fn = sum)
#
# # View the resulting data frame
# pivoted_df |> left_join(h2a_population |> filter(state == "Colorado"), by = "county") |> select(-state.y) |> view()


# Natural Disasters and Healthcare Facilities
weather_facility_load <- function() {
  disaster <- read_csv("~/internship/workspace/HICAHS_States_National_Risk_Index_Counties.csv") |>
    select(-1) |>
    select(!contains("coastal", ignore.case = TRUE),
           !contains("tsunami", ignore.case = TRUE),
           !contains("hurricane", ignore.case = TRUE)) |>
    select(-GlobalID)
  disaster[is.na(disaster)] <- 0
  assign("disaster", disaster, envir = globalenv())

  data_frames <- list(colorado, montana, north_dakota, south_dakota, utah, wyoming)
  full_data_list <- list()

  for (i in seq_along(states)) {
    state_disaster <- subset(disaster, state == states[i])
    full_data <- left_join(state_disaster, data_frames[[i]], by = 'county') |>
      select(-state.y) |>
      rename(state = state.x) |>
      select(where(is.numeric))
    #select(where(~ !all(replace_na(. == 0, FALSE))))
    full_data_list[[states[i]]] <- full_data
  }

  utah_full <- full_data_list[["Utah"]]
  utah_full <- utah_full |> mutate(id = 1:nrow(utah_full), .before = `Population (2020)`)

  colorado_full <- full_data_list[["Colorado"]]
  colorado_full <- colorado_full |> mutate(id = 1:nrow(colorado_full), .before = `Population (2020)`)

  wyoming_full <- full_data_list[["Wyoming"]]
  wyoming_full <- wyoming_full |> mutate(id = 1:nrow(wyoming_full), .before = `Population (2020)`)

  montana_full <- full_data_list[["Montana"]]
  montana_full <- montana_full |> mutate(id = 1:nrow(montana_full), .before = `Population (2020)`)

  northDakota_full <- full_data_list[["North Dakota"]]
  northDakota_full <- northDakota_full |> mutate(id = 1:nrow(northDakota_full), .before = `Population (2020)`)

  southDakota_full <- full_data_list[["South Dakota"]]
  southDakota_full <-  southDakota_full |> mutate(id = 1:nrow(southDakota_full), .before = `Population (2020)`)
  names(southDakota_full)[300:301] <- c("Rural_Health_Hospitals", "Critical_Access_Hospitals")

  # Add to global environment
  assign("colorado_full", colorado_full, envir = .GlobalEnv)
  assign("utah_full", utah_full, envir = .GlobalEnv)
  assign("wyoming_full", wyoming_full, envir = .GlobalEnv)
  assign("montana_full", montana_full, envir = .GlobalEnv)
  assign("northDakota_full", northDakota_full, envir = .GlobalEnv)
  assign("southDakota_full", southDakota_full, envir = .GlobalEnv)

}
weather_facility_load()

create_response_predictors <- function() {
  data_frames2 <- list(colorado_full, montana_full, northDakota_full, southDakota_full, utah_full, wyoming_full)
  response_variables <- c("Hospitals",
                          "Community_Clinics",
                          "Free_Standing_Emergency_Departments",
                          "Rehabilitation_Hospitals",
                          "Rural_Clinics",
                          "Rural_Health_Clinics",
                          "Critical_Access_Hospitals",
                          "Mammography",
                          "Home_Health_Agency",
                          "Assisted Living Facility - Type_I",
                          "Assisted Living Facility - Type_II",
                          "End Stage Renal Disease_Facility",
                          "Birthing Center",
                          "Abortion Clinic",
                          "Nursing Care Facility",
                          "Small Health Care_Facility",
                          "Small Health Care_Facility - Type_N",
                          "Personal Care Agency")

  predictor_variables <- setdiff(colnames(colorado_full), response_variables)[-c(1:5)]

  assign("response", response_variables, envir = .GlobalEnv)
  assign("predictors", predictor_variables, envir = .GlobalEnv)

}
create_response_predictors()


# corr_data <- data.frame(
#   Predictor = character(),
#   Response = character(),
#   Correlation = numeric(),
#   PValue = numeric()
#   )
#
# for (d in data_frames2) {
#   for(p in predictors) {
#     for (r in response) {
#       if (is.numeric(d[[p]]) && is.numeric(d[[r]])) {
#
#         cor_test <- cor.test(d[[r]], d[[p]], method = "spearman", exact = F)
#
#         corr_data <- rbind(corr_data, data.frame(
#           Predictor = as.character(p),
#           Response = as.character(r),
#           Correlation = round(cor_test$estimate, 3),
#           PValue = round(cor_test$p.value, 3)))
#       }
#     }
#   }
# }





healthFacility_envir_correlations <- function(data, response, predictors) {
  corr_df <- data.frame(
    Predictor = character(),
    Response = character(),
    Correlation = numeric(),
    PValue = numeric(),
    Significance = logical()
  )

  for (r in response) {
    for (predictor in predictors) {
      if (!(r %in% names(data)) || !(predictor %in% names(data))) {
        next
      }
      cor_test <- cor.test(data[[r]], data[[predictor]], method = "spearman", exact = FALSE)

      corr_df <- rbind(corr_df, data.frame(
        Predictor = predictor,
        Response = r,
        Correlation = round(cor_test$estimate, 2),
        PValue = round(cor_test$p.value, 3),
        Significance = ifelse(cor_test$p.value <= 0.05, TRUE, FALSE)
      ))
    }
  }

  corr_df <- corr_df |> arrange(desc(Correlation))
  return(corr_df)
}

colorado_corr <- healthFacility_envir_correlations(colorado_full, response, predictors)
utah_corr <- healthFacility_envir_correlations(utah_full, response, predictors)
wyoming_corr <- healthFacility_envir_correlations(wyoming_full, response, predictors)
northDakota_corr <- healthFacility_envir_correlations(northDakota_full, response, predictors)
southDakota_corr <- healthFacility_envir_correlations(southDakota_full, response, predictors)
montana_corr <- healthFacility_envir_correlations(montana_full, response, predictors)

