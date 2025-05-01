# Health Risk and Worker Density
#================================#
library(tidyverse)

# Import H-2A workers by county
h2a_by_county <- as.data.frame(read_csv("HICAHS/Data/NatDisasterR&I/H2AbyCounty.csv"))

# Data Cleaning

# Separating state totals into state_totals
state_totals <- h2a_by_county |> filter(grepl("Total", `State/ County`))
state_totals <- state_totals |> arrange(desc(`Total Workers H2A Certified`)) |> filter(!grepl("Grand", `State/ County`))
print(state_totals)

# Barplot of state totals
ggplot(data = state_totals, aes(x = `State/ County`, y = `Total Workers H2A Certified`)) +
  geom_bar(stat="identity", aes(fill = `State/ County`)) +
  geom_text(aes(label = `Total Workers H2A Certified`), vjust = 0.5) +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "H2A Workers per State", x = "State")

# Removing columnns with "Total" from original data
h2a_by_county_new <- h2a_by_county |> filter(!grepl("Total", `State/ County`))

# Separating into state and county columns
# inserting new column for state names
h2a_by_county_new <- h2a_by_county_new |>
  mutate(h2a_by_county_new, state = NA, .before = `State/ County`)
h2a_by_county_new <- h2a_by_county_new |>
  rename(county = `State/ County`)

# Reassigning states to counties
breaks <- c(0, 47, 102, 160, 219, 244, 269) # Indices
  labels <- c("Colorado", "Montana", "North Dakota", "South Dakota", "Utah", "Wyoming")
    h2a_by_county_new$state <- cut(seq_len(nrow(h2a_by_county_new)), breaks = breaks, labels = labels, right = TRUE)

h2a_by_county_new <- h2a_by_county_new |> filter(!grepl("Total", county))

# write_csv(h2a_by_county_new, file = "~/internship/workspace/Written Datasets/H2AWorkers_County_Clean.csv")
#______________________________________________________________________________________________________________________#
# Heat data
CountyMaxTemp_JUL23 <- as.data.frame(read_csv("HICAHS/Data/Heat_Ag_HumanRisk/CountyMaxTemp_JUL23.csv"))
CountyMaxTemp_AUG23 <- as.data.frame(read_csv("HICAHS/Data/Heat_Ag_HumanRisk/CountyMaxTemp_AUG23.csv"))

# adding Month column
CountyMaxTemp_JUL23 <- CountyMaxTemp_JUL23 |> mutate(month = "July")
CountyMaxTemp_AUG23 <- CountyMaxTemp_AUG23 |> mutate(month = "August")

# combining data sets
CountyMaxTemp_AUG_JUL_23 <- rbind(CountyMaxTemp_JUL23, CountyMaxTemp_AUG23)

# cleaning columns
CountyMaxTemp_AUG_JUL_23_new <- CountyMaxTemp_AUG_JUL_23 |> select(-ID)
CountyMaxTemp_AUG_JUL_23_new <- CountyMaxTemp_AUG_JUL_23_new |> rename(county = Name) |> rename(state = State)
CountyMaxTemp_AUG_JUL_23_new$county <- gsub(" County", "", CountyMaxTemp_AUG_JUL_23_new$county)

MaxTemp_H2AWorkers <- left_join(CountyMaxTemp_AUG_JUL_23_new, h2a_by_county_new,
                                          by = c("state", "county"),
                                          relationship = "many-to-many")

colnames(MaxTemp_H2AWorkers) <- tolower(colnames(MaxTemp_H2AWorkers))

# Data frame which combines heat data with the number of H2A workers per county
MaxTemp_H2AWorkers <- subset(MaxTemp_H2AWorkers, (!is.na(MaxTemp_H2AWorkers[,"total workers h2a certified"]))) |> arrange(state)
MaxTemp_H2AWorkers <- MaxTemp_H2AWorkers |>
  rename(mean = `1901-2000 mean`) |>
  rename(max_temp = value) |>
  rename(anomaly = `anomaly (1901-2000 base period)`)

# write_csv(MaxTemp_H2AWorkers, file = "~/internship/workspace/Written Datasets/Risk_H2AWorkers.csv")

# plotting
ggplot(data = MaxTemp_H2AWorkers, aes(x = max_temp, y = `total workers h2a certified`)) +
  geom_point(aes(color = state)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Maximum Temperature and Worker Density", x = "Maximum Temperature (F)", y = "Total H2A Certified Workers")
#______________________________________________________________________________________________________________________#
# Environmental risk data
disaster <- read_csv("~/internship/workspace/Data/HICAHS_States_National_Risk_Index_Counties.csv")

state_names <- c("Colorado", "Montana", "North Dakota", "South Dakota", "Utah", "Wyoming")
disaster <- filter(disaster, state %in% state_names)

# wildfire-specific data
disaster_fire <- bind_cols(disaster[2:21], select(disaster, contains("wildfire")))
disaster_fire <- select(disaster_fire, -contains("FIPS")) |> select(-abbrev)
colnames(disaster_fire) <- colnames(disaster_fire) |> tolower()
colnames(disaster_fire) <- gsub(" ", "_", names(disaster_fire))

# moving total workers into dataframe
disaster_fire$total_workers <- NA
disaster_fire$maximum_temperature <- NA
disaster_fire$mean_temperature <- NA

for (i in 1:nrow(MaxTemp_H2AWorkers)) {
  for (j in 1:nrow(disaster_fire)) {
    if (disaster_fire$county[j] == MaxTemp_H2AWorkers$county[i] & disaster_fire$state[j] == MaxTemp_H2AWorkers$state[i]) {
      disaster_fire$total_workers[j] = MaxTemp_H2AWorkers$`total workers h2a certified`[i]
      disaster_fire$maximum_temperature[j] = MaxTemp_H2AWorkers$max_temp[i]
      disaster_fire$mean_temperature[j] = MaxTemp_H2AWorkers$mean[i]
    }
  }
}


# SELECT * FROM "disaster_fire" WHERE "total_workers" IS NOT NULL
disaster_fire <- filter(disaster_fire, !is.na(total_workers))
colnames(disaster_fire) <- gsub("_-_", "_", colnames(disaster_fire))


# plotting H-2A population against wildfire risk
ggplot(data = disaster_fire, aes(y = total_workers, x = wildfire_hazard_type_risk_index_score)) +
  geom_point(aes(color = county)) +
  geom_smooth(method='lm', se=FALSE, col = "black", linewidth = 0.5) +
  facet_wrap(.~state, scales = "free") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Wildfire Risk Score and Worker Totals",
       x = "Wildfire Hazard Type Risk Index Score",
       y = "Total H2A Workers")
