# Health Risk and Worker Density
# Unconditional Correlations

# Import data
# merge and clean
# Correlation model
# grid?

# Packages
library(tidyverse)
# Clean environment
rm(list = ls())

# Import h2a by county data
h2a_by_county <- read_csv("HICAHS/Data/NatDisasterR&I/H2AbyCounty.csv")
state_totals = data.frame() # empty df

# Separating state total sums into state_totals
for (i in 1:nrow(h2a_by_county)) {
  if (any(grepl("\\(Total\\)", h2a_by_county[i, ]))) {
    state_totals <- rbind(state_totals, h2a_by_county[i, ])
  }
}
state_totals <- rbind(state_totals, h2a_by_county[nrow(h2a_by_county),]) # Grand total

# Removing totals from original data
total_rows <- apply(h2a_by_county, 1, function(row) any(grepl("\\(Total\\)", row)))
h2a_by_county_new <- h2a_by_county[!total_rows, ]

# Separating into state and county columns
# inserting new column for state names
h2a_by_county_new <- h2a_by_county_new |>
  mutate(h2a_by_county_new, state = NA, .before = `State/ County`)
h2a_by_county_new <- h2a_by_county_new |>
  rename(county = `State/ County`) |>
  rename(totalWorkersH2a = `Total Workers H2A Certified`) # Renaming columns for functionality/preference

# Reassigning states to counties
breaks <- c(0, 47, 102, 160, 219, 244, 269) # Indices
  labels <- c("Colorado", "Montana", "North Dakota", "South Dakota", "Utah", "Wyoming")
    h2a_by_county_new$state <- cut(seq_len(nrow(h2a_by_county_new)), breaks = breaks, labels = labels, right = TRUE)

h2a_by_county_new <- h2a_by_county_new |> filter(row_number() <= n()-1)

write_csv(h2a_by_county_new, file = "~/internship/workspace/h2a_by_county_new.csv")
#______________________________________________________________________________________________________________________#

# TODO: fire and heat exposure

