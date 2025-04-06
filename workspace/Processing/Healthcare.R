library(tidyverse)
library(readxl)
library(ggplot2)
library(ggcorrplot)


state_facility_load <- function() {
  # health risks
  health <- read_csv("~/internship/workspace/PLACES__Local_Data_for_Better_Health__County_Data_2024_release_20250306.csv")
  colnames(health) <- tolower(names(health))

  states <- c("Colorado", "Montana", "North Dakota", "South Dakota", "Utah", "Wyoming")

  health <- filter(health, state %in% states) |> arrange(stateabbr, county) |> select(-locationid)

  # health facilities
  facility_files <- list(
    colorado = "~/internship/workspace/Health Facility Data/ColoradoHealth24_xlsx.xlsx",
    wyoming = "~/internship/workspace/Health Facility Data/WyomingHealth24_xlsx.xlsx",
    south_dakota = "~/internship/workspace/Health Facility Data/SouthDakotaHealth24_xlsx.xlsx",
    north_dakota = "~/internship/workspace/Health Facility Data/NorthDakotaHealth24_xlsx.xlsx",
    utah = "~/internship/workspace/Health Facility Data/UtahHealth24_xlsx.xlsx",
    montana = "~/internship/workspace/Health Facility Data/MontanaHealth24_xlsx.xlsx"
  )

  facilities <- lapply(names(facility_files), function(state) {
    file <- facility_files[[state]]
    data <- read_excel(file, sheet = ifelse(state %in% c("north_dakota", "montana"), 1, 2))

    if (state == "wyoming") {
      data$county <- gsub(" County", "", data$county)
    }

    if (state == "montana") {
      data$county <- str_to_title(data$county)
    }
    mhc_count$state <- tolower(mhc_count$state)
    data <- data |> left_join(mhc_count, by = c("state", "county"))
    data$MigrantHealthCenters[is.na(data$MigrantHealthCenters)] <- 0

    assign(state, data, envir = .GlobalEnv)
  })

  # H2-A population
  h2a_population <- read_csv("~/internship/workspace/Written Datasets/h2a_by_county_new.csv") |> arrange(state, county)

  assign("health", health, envir = .GlobalEnv)
  assign("h2a_population", h2a_population, envir = .GlobalEnv)
  assign("states", states, envir = .GlobalEnv)
}


migrant_health_centers <- function() {
  # migrant health centers specifically
  ncfh <- read_csv("~/internship/workspace/migrant_health_centers_ncfh.csv")
  ncfh$county <- str_to_title(ncfh$county)
  ncfh$state <- str_to_title(ncfh$state)
  assign("ncfh", ncfh, envir = .GlobalEnv)

  mhc_count <- ncfh |>
    group_by(state, county) |>
    summarise(MigrantHealthCenters = n(), .groups = "drop")
  assign("mhc_count", mhc_count, envir = .GlobalEnv)
}


healthcare <- function() {
  # Helper function to process state health data
  process_state_health <- function(state_name, state_data) {
    state_health <- subset(health, state == state_name)
    state_health <- state_health |>
      left_join(state_data, by = "county") |>
      select(-state.y) |>
      rename(state = state.x) |>
      left_join(h2a_population, by = c("county", "state"))

    col_order <- c(head(names(state_health), -2), tail(names(state_health), 2)[2], tail(names(state_health), 2)[1])
    state_health <- state_health[, col_order]

    state_health
  }

  # Process each state's health data
  co_health <- process_state_health("Colorado", colorado)
  wy_health <- process_state_health("Wyoming", wyoming)
  sdkt_health <- process_state_health("South Dakota", south_dakota)
  ndkt_health <- process_state_health("North Dakota", north_dakota)
  ut_health <- process_state_health("Utah", utah)
  mt_health <- process_state_health("Montana", montana)

  # Add to global environment
  assign("co_health", co_health, envir = .GlobalEnv)
  assign("ut_health", ut_health, envir = .GlobalEnv)
  assign("wy_health", wy_health, envir = .GlobalEnv)
  assign("mt_health", mt_health, envir = .GlobalEnv)
  assign("ndkt_health", ndkt_health, envir = .GlobalEnv)
  assign("sdkt_health", sdkt_health, envir = .GlobalEnv)
}


stateFacilityCorrelations <- function() {
  # Initialize all 6 data frames for storing correlation coefficients
  init_corr_df <- function() {
    data.frame(
      Predictor = character(),
      Correlation = numeric(),
      PValue = numeric(),
      Significance = numeric()
    )
  }

  co_corr <- init_corr_df()
  mt_corr <- init_corr_df()
  ndkt_corr <- init_corr_df()
  sdkt_corr <- init_corr_df()
  wy_corr <- init_corr_df()
  ut_corr <- init_corr_df()

  # Helper function to calculate correlations
  calculate_correlations <- function(health_data, state_name) {
    predictors <- names(health_data[, (which(names(health_data) == "geolocation") + 1):(which(names(health_data) == "total_workers_h2a") - 1)])
    corr_df <- init_corr_df()
    for (predictor in predictors) {
      cor_test <- cor.test(health_data[[predictor]], health_data$total_workers_h2a, method = "spearman", exact = FALSE, use = "pairwise.complete.obs")
      corr_df <- rbind(corr_df, data.frame(
        Predictor = predictor,
        Correlation = round(cor_test$estimate, 3),
        PValue = cor_test$p.value
      ))
    }
    corr_df <- corr_df |>
      mutate(Variable = "total_workers_h2a", .before = Predictor) |>
      mutate(State = state_name, .before = Variable)
    corr_df$Significance <- ifelse(corr_df$PValue <= 0.05, TRUE, FALSE)
    corr_df
  }

  # Calculate correlations for each state
  co_corr <- calculate_correlations(co_health, "Colorado")
  mt_corr <- calculate_correlations(mt_health, "Montana")
  ndkt_corr <- calculate_correlations(ndkt_health, "North Dakota")
  sdkt_corr <- calculate_correlations(sdkt_health, "South Dakota")
  wy_corr <- calculate_correlations(wy_health, "Wyoming")
  ut_corr <- calculate_correlations(ut_health, "Utah")

  # Binding the data together
  h2aPopulationCorrelations <- rbind(co_corr, wy_corr, ndkt_corr, sdkt_corr, ut_corr, mt_corr) |> dplyr::arrange(State, Correlation)
  assign("h2aPopulationCorrelations", h2aPopulationCorrelations, envir = globalenv())
}


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



weather_facility_load <- function() {
  # Load and process disaster data
  disaster <- read_csv("~/internship/workspace/Written Datasets/Disaster_clean.csv")
  disaster <- disaster |> select(-grep("coastal|Tsunami|hurricane|volcanic", colnames(disaster), ignore.case = TRUE))
  disaster[is.na(disaster)] <- 0
  colnames(disaster) <- gsub(" ", "", colnames(disaster))

  assign("disaster", disaster, envir = globalenv())

  # Prepare a list of data frames for each state
  data_frames <- list(colorado, montana, north_dakota, south_dakota, utah, wyoming)
  full_data_list <- list()

  # Process data for each state
  for (i in seq_along(states)) {
    state_disaster <- subset(disaster, state == states[i])
    full_data <- left_join(state_disaster, data_frames[[i]], by = 'county') |>
      select(-state.y) |>
      rename(state = state.x) |>
      select(where(is.numeric))
    full_data[is.na(full_data)] <- 0
    full_data_list[[states[i]]] <- full_data
  }

  # Add ID column and filter out non-numeric columns
  add_id_and_filter_numeric <- function(df) {
    df |> mutate(id = 1:nrow(df), .before = `Population2020`) |> select(where(is.numeric))
  }

  full_data_list <- lapply(full_data_list, add_id_and_filter_numeric)


  # Assign individual state data frames
  assign("utah_full", full_data_list[["Utah"]], envir = .GlobalEnv)
  assign("colorado_full", full_data_list[["Colorado"]], envir = .GlobalEnv)
  assign("wyoming_full", full_data_list[["Wyoming"]], envir = .GlobalEnv)
  assign("montana_full", full_data_list[["Montana"]], envir = .GlobalEnv)
  assign("northDakota_full", full_data_list[["North Dakota"]], envir = .GlobalEnv)
  assign("southDakota_full", full_data_list[["South Dakota"]], envir = .GlobalEnv)

  # Clean column names in the final data frames
  clean_column_names <- function(df) {
    colnames(df) <- gsub("_", "", colnames(df))
    colnames(df) <- gsub("-", "", colnames(df))
    colnames(df) <- gsub(" ", "", colnames(df))
    df
  }

  # colorado_full <- colorado_full |> rename(MigrantHealthCenters = migranthealthcenters)
  # utah_full <- utah_full |> rename(MigrantHealthCenters = migranthealthcenters)
  # wyoming_full <- wyoming_full |> rename(MigrantHealthCenters = migranthealthcenters)
  # northDakota_full <- northDakota_full |> rename(MigrantHealthCenters = migranthealthcenters)
  # southDakota_full <- southDakota_full |> rename(MigrantHealthCenters = migranthealthcenters)
  # montana_full <- montana_full |> rename(MigrantHealthCenters = migranthealthcenters)

  # Apply cleaning function to each state data frame
  assign("utah_full", clean_column_names(utah_full), envir = .GlobalEnv)
  assign("colorado_full", clean_column_names(colorado_full), envir = .GlobalEnv)
  assign("wyoming_full", clean_column_names(wyoming_full), envir = .GlobalEnv)
  assign("montana_full", clean_column_names(montana_full), envir = .GlobalEnv)
  assign("northDakota_full", clean_column_names(northDakota_full), envir = .GlobalEnv)
  assign("southDakota_full", clean_column_names(southDakota_full), envir = .GlobalEnv)
}



create_response_predictors <- function() {
  # data_frames2 <- list(colorado_full, montana_full, northDakota_full, southDakota_full, utah_full, wyoming_full)
  response_variables <- c("Hospitals",
                          "CommunityClinics",
                          "FreeStandingEmergencyDepartments",
                          "RehabilitationHospitals",
                          "RuralClinics",
                          "RuralHealthClinics",
                          "CriticalAccessHospitals",
                          "Mammography",
                          "HomeHealthAgency",
                          "AssistedLivingFacilityTypeI",
                          "AssistedLivingFacilityTypeII",
                          "EndStageRenalDiseaseFacility",
                          "BirthingCenter",
                          "AbortionClinic",
                          "NursingCareFacility",
                          "SmallHealthCareFacility",
                          "SmallHealthCareFacilityTypeN",
                          "PersonalCareAgency",
                          "MigrantHealthCenters")

  predictor_variables <- setdiff(colnames(colorado_full), response_variables)[-c(1:5)]

  assign("response", response_variables, envir = .GlobalEnv)
  assign("predictors", predictor_variables, envir = .GlobalEnv)

}


healthFacility_envir_correlations <- function(data, response, predictors) {
  corr_df <- data.frame(
    Predictor = character(),
    Response = character(),
    Correlation = numeric(),
    PValue = numeric(),
    Sig = logical()
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
        Correlation = round(cor_test$estimate, 3),
        PValue = round(cor_test$p.value, 3),
        Sig = ifelse(cor_test$p.value <= 0.05, TRUE, FALSE)
      ))
    }
  }

  corr_df <- corr_df |> arrange(desc(Correlation))
  return(corr_df)
}


#data_frames2 <- list(colorado_full, montana_full, northDakota_full, southDakota_full, utah_full, wyoming_full)


# Calling functions
migrant_health_centers()
state_facility_load()
healthcare()
stateFacilityCorrelations()
weather_facility_load()
create_response_predictors()

colorado_corr <- healthFacility_envir_correlations(colorado_full, response, predictors) |> na.omit()
colorado_corr_filtered <- colorado_corr |> filter(Sig == TRUE)
knitr::kable(colorado_corr_filtered)
cat("Number of Correlations Not Statistically Signifcant: ", nrow(colorado_corr) - nrow(colorado_corr_filtered))


data_for_feature_selection <- function(predictor) {
  colorado_cut <- colorado_full[,1:which(names(colorado_full) == predictor)]
  montana_cut <- montana_full[,1:which(names(montana_full) == predictor)]
  utah_cut <- utah_full[,1:which(names(utah_full) == predictor)]
  utah_cut <- utah_cut |> dplyr::select(1:89, predictor)
  wyoming_cut <- wyoming_full[,1:which(names(wyoming_full) == predictor)]
  wyoming_cut <- wyoming_cut |> dplyr::select(1:89, predictor)
  northDakota_cut <- northDakota_full[,1:which(names(northDakota_full) == predictor)]
  northDakota_cut <- northDakota_cut |> select(1:89, predictor)
  southDakota_cut <- southDakota_full[,1:which(names(southDakota_full) == predictor)]
  southDakota_cut <- southDakota_cut |> dplyr::select(1:89, predictor)

  feature_selection_data <- rbind(colorado_cut, montana_cut, utah_cut, wyoming_cut, northDakota_cut, southDakota_cut)
  assign("feature_selection_data", feature_selection_data, envir = .GlobalEnv)
}
data_for_feature_selection("Hospitals")
