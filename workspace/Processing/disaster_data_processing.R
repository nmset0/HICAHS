# Reduce redundancy
# Remove variables with little meaning or effect
# Clean data structure


# Packages
library(readr)
library(dplyr)

# Clean .GlobalEnv
# rm(list = ls())

# Read data
disaster_raw <- read_csv("~/internship/workspace/HICAHS_States_National_Risk_Index_Counties.csv")

# Create list of states
state_names <- c("Colorado", "Montana", "North Dakota", "South Dakota", "Utah", "Wyoming")

# Fill NAs
disaster_raw[is.na(disaster_raw)] <- 0

# Clean column names
colnames(disaster_raw) <- gsub("[()]", "", colnames(disaster_raw))
  colnames(disaster_raw) <- gsub("\\$", "", colnames(disaster_raw))
    colnames(disaster_raw) <- gsub("_ | -", "", colnames(disaster_raw))
     colnames(disaster_raw) <- gsub(" ", "", colnames(disaster_raw))
        colnames(disaster_raw) <- gsub("__", "", colnames(disaster_raw))

# Sort numeric and categorical variables to use later
numeric_cols <- disaster_raw |> select(where(is.numeric))
categoricl_cols <- disaster_raw |> select(setdiff(names(disaster_raw), names(numeric_cols)))

# Mass row and column removal
disaster_2 <- filter(disaster_raw, state %in% state_names) # Retrieve HICAHS states
  disaster_2 <- lapply(disaster_2, function(x) if(is.numeric(x)) round(x, 3) else x) # Round numeric columns to 3 digits
    disaster_2 <- as.data.frame(disaster_2) # Convert large list back to df
      # Remove empty columns
    for (i in colnames(disaster_2)) {
      if (is.numeric(disaster_2[[i]])) {
        if (all(disaster_2[[i]] == 0)) {
          disaster_2[[i]] <- NULL
        }
      }
    }
    disaster_2 <- disaster_2 |> select(-grep("Percentile", colnames(disaster_2), ignore.case = T))
          # VST defined by FEMA: might be unnecessary. Variables are also substantially larger than the rest of the data.
          disaster_2 <- disaster_2 |> select(-grep("PopulationEquivalence", colnames(disaster_2), ignore.case = T))
            # Remove irrelevant natural disasters
            disaster_2 <- disaster_2 |> select(-grep("earthquake|coastal|Tsunami|hurricane|volcanic|CoastalFlooding", colnames(disaster_2), ignore.case = T))
              # Remove columns where sd(column) = 0
              disaster_2 <- disaster_2[, !apply(disaster_2, 2, function(col) length(unique(col)) == 1)]

# Specific variable removal                                                 GIS?
disaster_3 <- disaster_2 |> select(-GlobalID, -NationalRiskIndexID, -ShapeArea, -ShapeLength, -CommunityRiskFactorValue)
disaster_3 <- disaster_3 |> mutate(id = 1:nrow(disaster_3), .before = state)
disaster_3 <- disaster_3 |> rename(AreaSqMi = Areasqmi)

# Df of "Totals" columns
disaster_4 <- disaster_2 |>
  select(names(disaster_2)[2:4], matches("Total", ignore.case = TRUE))

disaster_5 <- disaster_raw |>
  select(names(disaster_raw)[2:4], matches("NumberofEvents|numberofevents|events|numberof", ignore.case = TRUE))
disaster_5 <- disaster_5[, colSums(disaster_5 != 0) > 0]

disaster_6 <- disaster_2 |>
  select(names(disaster_2)[2:8], matches("Wildfire|Hail|Lightning|Tornado|Heat|Cold|WinterWeather|Drought|Ice|Landslide|Riverine|StrongWind|Avalanche", ignore.case = TRUE))


write_csv(disaster_3, file= "~/internship/workspace/Written Datasets/disaster_clean.csv")
write_csv(disaster_6, file = "~/internship/workspace/Written Datasets/disaster_cut_clean.csv")







