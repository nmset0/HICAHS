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
h2a_by_county <- as.data.frame(read_csv("HICAHS/Data/NatDisasterR&I/H2AbyCounty.csv"))
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
# Heat data
CountyMaxTemp_JUL23 <- as.data.frame(read_csv("HICAHS/Data/Heat_Ag_HumanRisk/CountyMaxTemp_JUL23.csv"))
CountyMaxTemp_AUG23 <- as.data.frame(read_csv("HICAHS/Data/Heat_Ag_HumanRisk/CountyMaxTemp_AUG23.csv"))

CountyMaxTemp_JUL23 <- CountyMaxTemp_JUL23 |> mutate(month = "July")
CountyMaxTemp_AUG23 <- CountyMaxTemp_AUG23 |> mutate(month = "August")

CountyMaxTemp_AUG_JUL_23 <- rbind(CountyMaxTemp_JUL23, CountyMaxTemp_AUG23)

CountyMaxTemp_AUG_JUL_23_new <- subset(CountyMaxTemp_AUG_JUL_23, select = -c(ID, State))

CountyMaxTemp_AUG_JUL_23_new$Name <- gsub(" County", "", CountyMaxTemp_AUG_JUL_23_new$Name)

MaxTemp_H2AWorkers <- left_join(CountyMaxTemp_AUG_JUL_23_new, h2a_by_county_new,
                                          by = c("Name" = "county"),
                                          relationship = "many-to-many")



# Data frame which combines heat data with the number of H2A workers per county
MaxTemp_H2AWorkers <- subset(MaxTemp_H2AWorkers, (!is.na(MaxTemp_H2AWorkers[,"totalWorkersH2a"]))) |> arrange(state)
MaxTemp_H2AWorkers <- MaxTemp_H2AWorkers |> rename(county = Name)


# barplot of state totals
ggplot(data = state_totals[1:6,], aes(x = `State/ County`, y = `Total Workers H2A Certified`)) +
  geom_bar(stat="identity", aes(fill = `State/ County`)) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Number of Recorded H2A Workers per State", x = "State")



