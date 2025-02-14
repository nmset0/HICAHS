library(tidyverse)
library(readr)

rm(list=ls())

# Importing data (290 datasets)
disaster <- read_csv("~/internship/workspace/HICAHS_States_National_Risk_Index_Counties.csv")

base_path <- "C:/Users/natha/OneDrive/Documents/internship/workspace/Ag Output Data"
states <- c("Colorado", "Wyoming", "North_Dakota", "South_Dakota", "Utah", "Montana")

source("~/internship/workspace/Processing/ag_data_function.R")
ag_output <- load_ag_output_data(base_path, states)

# Begin Cleaning
#-----------------------------Agricultural Output Data----------------------------------#
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

# preparing for data merge: adding rows to match `county` columns;
# changing row order to match `county`
ag_output_wide <- ag_output_wide |>
  add_row(county = "chaffee", state = "colorado", .before = 9) |>
    add_row(county = "san juan", state = "colorado", .before = 57) |>
      slice(c(1:20, 22, 21, 23:n())) |>
        slice(c(1:33, 35, 34, 36:n()))
#-----------------------------Natural Disaster Data--------------------------------------#
# colnames to lower case
disaster <- as.data.frame(lapply(disaster, function(x) {
  if (is.character(x)) tolower(x) else x
}))

disaster <- disaster |> select(-National.Risk.Index.ID)
# preparing to merge: changing entry in disaster data to match agriculture data
# (otherwise whole row will become NA when matching order (below))
disaster$county[disaster$county == "lamoure"] <- "la moure"

disaster <- disaster |> select(-matches("flooding|earthquake|hurricane|tsunami|volcanic", ignore.case = TRUE))
#----------------------------------------Merge Data---------------------------------------#
# preparing to merge: matching order of datasets based on `county`
disaster <- disaster[match(paste(ag_output_wide$state, ag_output_wide$county),
                           paste(disaster$state, disaster$county)), ]
# End Cleaning

# Verify:
# sum((disaster$county == ag_output_wide$county) == FALSE)

# Dataset merge:
shared_cols <- intersect(names(disaster), names(ag_output_wide))
  ag_output_unique <- ag_output_wide |> select(-all_of(shared_cols))
    output_risk_combined <- cbind(disaster, ag_output_unique)

view(ag_output_wide)
view(disaster)
view(output_risk_combined)

# write_csv(output_risk_combined, file = "~/internship/workspace/output_risk_combined.csv")


