library(tidyverse)
library(readr)
library(data.table)

rm(list=ls())

# Importing data (290 datasets)
disaster <- read_csv("~/internship/workspace/HICAHS_States_National_Risk_Index_Counties.csv")

states <- c("Colorado", "Wyoming", "North_Dakota", "South_Dakota", "Utah", "Montana")
base_path <- "C:/Users/natha/OneDrive/Documents/internship/workspace/Ag Output Data"

state_outputs <- list()

for(state in states) {
  setwd(file.path(base_path, state))

  files <- list.files(pattern = "*.csv")
  dataset <- do.call(rbind, lapply(files, fread))

  state_outputs[[state]] <- as.data.frame(unclass(dataset))

  rm(files, dataset)
}

ag_output <- do.call(rbind, state_outputs)

rm(state_outputs, state, states, base_path)

setwd("C:/Users/natha/OneDrive/Documents/internship")

for (i in 1:nrow(ag_output)) {
  if (!is.na(ag_output$domain.category[i]) && ag_output$domain.category[i] != "") {
    ag_output$data.item[i] <- ag_output$domain.category[i]
  }
}

ag_output <- ag_output %>% select(-domain.category, -state.fips, -commodity, -county.code)

ag_output <- as.data.frame(lapply(ag_output, function(x) {
  if (is.character(x)) tolower(x) else x
}))

ag_output_wide <- ag_output %>%
  pivot_wider(names_from = data.item, values_from = value)

colnames(ag_output_wide) <- gsub(":", "", colnames(ag_output_wide))
  colnames(ag_output_wide) <- gsub("-", "", colnames(ag_output_wide))
    colnames(ag_output_wide) <- gsub(",", "", colnames(ag_output_wide))
      colnames(ag_output_wide) <- gsub("/", "", colnames(ag_output_wide))
        colnames(ag_output_wide) <- gsub(" ", "_", colnames(ag_output_wide))
          colnames(ag_output_wide) <- gsub("__", "_", colnames(ag_output_wide))

# view(ag_output_wide)

ag_output_wide <- ag_output_wide |>
  add_row(county = "chaffee", state = "colorado", .before = 9) |>
    add_row(county = "san juan", state = "colorado", .before = 57) |>
      slice(c(1:20, 22, 21, 23:n())) |>
        slice(c(1:33, 35, 34, 36:n()))
#-------------------------------------------------------------------------------------------------------------#

disaster <- as.data.frame(lapply(disaster, function(x) {
  if (is.character(x)) tolower(x) else x
}))

disaster <- disaster |> select(-National.Risk.Index.ID)
disaster$county[disaster$county == "lamoure"] <- "la moure"

# view(disaster)
#-------------------------------------------------------------------------------------------------------------#
# combining datasets for correlations

disaster <- disaster[match(paste(ag_output_wide$state, ag_output_wide$county),
                           paste(disaster$state, disaster$county)), ]

shared_cols <- intersect(names(disaster), names(ag_output_wide))
  ag_output_unique <- ag_output_wide %>% select(-all_of(shared_cols))
    output_risk_combined <- cbind(disaster, ag_output_unique)

