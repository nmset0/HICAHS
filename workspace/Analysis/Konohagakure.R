library(tidyverse)
library(readxl)
library(ggcorrplot)
library(leaflet)
library(leaflet.extras)
library(magrittr)
library(sf)
library(knitr)

opts_chunk$set(echo = F, eval = T, warning = F, message = F)

# Data:
H2A <- read_csv("~/internship/workspace/Written Datasets/h2a_by_county_new.csv") # Worker density
MHC <- read_csv("~/internship/workspace/migrant_health_centers_ncfh.csv") # Migrant health centers
ENV <- read_csv("~/internship/workspace/Written Datasets/disaster_cut_clean.csv") # Natural disaster and weather risk
AGO <- read_csv("~/internship/workspace/Written Datasets/ag_output_clean.csv") # Agricultural output
JUL23 <- read_csv("~/internship/HICAHS/Data/Heat_Ag_HumanRisk/CountyMaxTemp_JUL23.csv") # Maximum temperature data for July 2023
AUG23 <- read_csv("~/internship/HICAHS/Data/Heat_Ag_HumanRisk/CountyMaxTemp_AUG23.csv") # Maximum temperature data for August 2023

# Healthcare facilities in each state
# (Cannot bind due to differing number of columns)
COH <- read_excel("~/internship/workspace/Health Facility Data/ColoradoHealth24_xlsx.xlsx", sheet = 2)
WYH <- read_excel("~/internship/workspace/Health Facility Data/WyomingHealth24_xlsx.xlsx", sheet = 2)
SDH <- read_excel("~/internship/workspace/Health Facility Data/SouthDakotaHealth24_xlsx.xlsx", sheet = 2)
NDH <- read_excel("~/internship/workspace/Health Facility Data/NorthDakotaHealth24_xlsx.xlsx", sheet = 1)
UTH <- read_excel("~/internship/workspace/Health Facility Data/UtahHealth24_xlsx.xlsx", sheet = 2)
MTH <- read_excel("~/internship/workspace/Health Facility Data/MontanaHealth24_xlsx.xlsx", sheet = 1)

# Shape file for mapping
HICAHS_SHP <- invisible(st_read("~/internship/workspace/tl_2024_us_state/tl_2024_us_state.shp", quiet = TRUE))
COUNTY_SHP <- invisible(st_read("~/internship/workspace/tl_2024_us_county/tl_2024_us_county.shp", quiet = TRUE))

response <- character()
predictor <- character()

ENV$county[ENV$county == "lamoure"] <- "la moure"
ENV <- ENV |> select(-matches("coastal|hurricane|tsunami|volcanic", ignore.case = TRUE))
AGO[] <- lapply(AGO, function(col) gsub(",", "", col))

WYH$county <- gsub(" County", "", WYH$county)
MTH$county <- str_to_title(MTH$county)

COH$state <- str_to_title(COH$state)
WYH$state <- str_to_title(WYH$state)
SDH$state <- str_to_title(SDH$state)
NDH$state <- str_to_title(NDH$state)
UTH$state <- str_to_title(UTH$state)
MTH$state <- str_to_title(MTH$state)

JUL23$Name <- gsub(" County", "", JUL23$Name)
AUG23$Name <- gsub(" County", "", AUG23$Name)

JUL23 <- JUL23 |> rename(Jul2023_MaxTemp = Value)
AUG23 <- AUG23 |> rename(Aug2023_MaxTemp = Value)

JUL23 <- JUL23 |> mutate(year = 2023, month = "July", .before = Name)
AUG23 <- AUG23 |> mutate(year = 2023, month = "August", .before = Name)

JUL23 <- JUL23 |> rename(county = Name) |> rename(state = State) |> rename(`July1901-2000Mean` = `1901-2000 Mean`)
AUG23 <- AUG23 |> rename(county = Name) |> rename(state = State) |> rename(`Aug1901-2000Mean` = `1901-2000 Mean`)

# Column uniformity
H2A$county <- str_to_title(H2A$county)
MHC$county <- str_to_title(MHC$county)
ENV$county <- str_to_title(ENV$county)
AGO$county <- str_to_title(AGO$county)

H2A$state <- str_to_title(H2A$state)
MHC$state <- str_to_title(MHC$state)
ENV$state <- str_to_title(ENV$state)
AGO$state <- str_to_title(AGO$state)
AUG23$state <- str_to_title(AUG23$state)
JUL23$state <- str_to_title(JUL23$state)

# Adding latitude and longitude for mapping
MHC <- MHC |> separate(geolocation, into = c("latitude", "longitude"), sep = ",", convert = TRUE)

# Sum number of migrant health facilities per county
MHC_sum <- MHC |>
  group_by(state, county) |>
  summarise(MigrantHealthCenters = n(), .groups = "drop")


# Binding all data together into one data set to work with:
JOIN <- left_join(ENV, H2A, by = c("state", "county")) # Join H-2A worker totals and natural disaster data sets
JOIN <- left_join(JOIN, AGO, by = c("state", "county")) # Join agricultural output data set
JOIN <- left_join(JOIN, MHC_sum, by = c("state", "county")) # Join migrant health center counts
JOIN <- JOIN |>
  left_join(COH, by = c("state", "county")) |>
  left_join(WYH, by = c("state", "county")) |>
  left_join(SDH, by = c("state", "county")) |>
  left_join(NDH, by = c("state", "county")) |>
  left_join(UTH, by = c("state", "county")) |>
  left_join(MTH, by = c("state", "county"))

JOIN <- JOIN |> mutate(MigrantHealthCenters = ifelse(is.na(MigrantHealthCenters), 0, MigrantHealthCenters))
JOIN <- JOIN |> select(-grep("abbrev", colnames(JOIN), ignore.case = T))

JOIN <- JOIN |> mutate(Hospitals = coalesce(Hospitals.x, Hospitals.y, Hospitals.x.x, Hospitals.y.y, Hospitals.x.x.x, Hospitals.y.y.y))
JOIN <- JOIN |> select(-Hospitals.x, -Hospitals.y, -Hospitals.x.x, -Hospitals.y.y, -Hospitals.x.x.x, -Hospitals.y.y.y)

JOIN <- JOIN |> mutate(`RuralHealthClinics` = coalesce(`Rural_Clinics`, `Rural Health Clinics`, Rural_Health_Clinics.x, Rural_Health_Clinics.y))
JOIN <- JOIN |> select(-`Rural_Clinics`, -`Rural Health Clinics`, -Rural_Health_Clinics.x, -Rural_Health_Clinics.y)

JOIN <- JOIN |> mutate(`CriticalAccessHospitals` = coalesce(`Critical Access Hospitals`, Critical_Access_Hospitals))
JOIN <- JOIN |> select(-`Critical Access Hospitals`, -Critical_Access_Hospitals)

JOIN$county[JOIN$county == "lamoure"] <- "la moure"

JOIN$H2A_workers[is.na(JOIN$H2A_workers)] <- 0


# JOIN <- JOIN |>
#   group_by(state) |>
#   mutate(H2AStateTotal = sum(H2A_workers, na.rm = TRUE))
#---------------------------------------------------------------------------------------------------------------#
JOIN_CUT <- JOIN |>
  select(
    "state",
    "county",
    "Population2020",
    "BuildingValue",
    "AgricultureValue",
    "Areasqmi",
    "H2A_workers",
    "JOIN_CUT$latitude",
    "JOIN_CUT$longitude",
    grep(
      "income|sales|farm_sales|income_net_|commodity_totals|crop_totals_sales|drought|wildfire|heat|heatwave",
      colnames(JOIN),
      ignore.case = TRUE
    ),
    349:length(JOIN)
  )

# Very simplified subset of risk variables
JOIN_CUT <- JOIN_CUT |> select(-grep("Expected|wheat|cattle|hogs|chickens|receipts|historic|index", colnames(JOIN_CUT), ignore.case = T))
# Filling NAs
JOIN_CUT[sapply(JOIN_CUT, is.numeric)] <- lapply(JOIN_CUT[sapply(JOIN_CUT, is.numeric)], function(x) ifelse(is.na(x), 0, x))
# Deleting empty columns
JOIN_CUT <- JOIN_CUT[, colSums(!is.na(JOIN_CUT)) > 0]

JOIN_CUT <- JOIN_CUT |>
  left_join(JUL23 |> select(state, county, Jul2023_MaxTemp, `July1901-2000Mean`), by = c("state", "county")) |>
  left_join(AUG23 |> select(state, county, Aug2023_MaxTemp, `Aug1901-2000Mean`), by = c("state", "county"))

JOIN_CUT <- JOIN_CUT |> arrange(state, county)



GGDAT1 <- MHC_sum |>
  group_by(state) |>
  summarise(total = sum(MigrantHealthCenters, na.rm = TRUE))

# GGDAT1 <- rbind(GGDAT1, c("South Dakota", 0), c("Wyoming", 0))

GGDAT1$state <- factor(GGDAT1$state, levels = c("Colorado", "Montana", "Utah", "North Dakota"))

GGPLOT1 <- ggplot(data = GGDAT1, aes(y = state, x = as.numeric(total))) +
  geom_bar(stat = "identity", color = "black", fill = "darkgreen", width = .75) +
  labs(y = "State", x = "Migrant Health Centers", title = "Figure 2: Migrant Health Centers Per State (NCFH 2023)") +
  scale_x_continuous(breaks = seq(from = 0, to = 40, by = 2)) +
  geom_text(aes(label = total), hjust = 1.15, color = "white") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

GGDAT2 <- H2A |>
  group_by(state) |>
  summarise(total = sum(H2A_workers, na.rm = TRUE))

GGDAT2$state <- factor(GGDAT2$state, levels = c("Colorado", "North Dakota", "South Dakota", "Montana", "Utah", "Wyoming"))

GGPLOT2 <- ggplot(data = GGDAT2, aes(y = state, x = as.numeric(total))) +
  geom_bar(stat = "identity", color = "black", fill = "skyblue", width = .75) +
  labs(y = "State", x = "Worker Count", title = "Figure 1: H-2A Workers") +
  scale_x_continuous(breaks = seq(from = 0, to = 5000, by = 500)) +
  geom_text(aes(label = total), hjust = 1.1, color = "white") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

GGPLOT2
GGPLOT1




selected_states <- HICAHS_SHP[HICAHS_SHP$NAME %in% c("Wyoming", "Colorado", "Montana", "Utah", "North Dakota", "South Dakota"), ]
selected_counties <- COUNTY_SHP[COUNTY_SHP$STATEFP %in% selected_states$STATEFP, ] |> arrange(STATEFP, NAME)



JOIN_CUT_CLEAN <- JOIN_CUT |> filter(county != "Lamoure")
JOIN_CUT_sf <- st_as_sf(JOIN_CUT_CLEAN, coords = c("JOIN_CUT$longitude", "JOIN_CUT$latitude"), crs = 4326)


color_palette <- colorNumeric(palette = c("yellow", "orange", "red"), domain = JOIN_CUT_sf$H2A_workers)
map <- leaflet(MHC) |>
  addTiles() |>
  setView(lng = -104.993498, lat = 42.468594, zoom = 5.45) |>
  addProviderTiles(providers$Esri.WorldTopoMap) |>
  addPolygons(data = selected_states,
              fill = FALSE,
              color = "black",
              weight = 0.5,
              opacity = 1) |>
  addPolygons(data = selected_counties,
              fillColor = ~color_palette(JOIN_CUT_sf$H2A_workers),
              fillOpacity = 0.7,
              color = "darkgrey",
              weight = 0.5,
              opacity = 1) |>
  addCircleMarkers(lng = ~longitude, lat = ~latitude,
                   fill = TRUE,
                   fillColor = "blue",
                   fillOpacity = 1,
                   stroke = T,
                   radius = 2) |>
  addLegend(pal = color_palette, values = JOIN_CUT_sf$H2A_workers, title = "H-2A Worker Population", position = "bottomright")

map



H2AMHC <- H2A |> left_join(MHC_sum, by = c("state", "county"))
H2AMHC$MigrantHealthCenters[is.na(H2AMHC$MigrantHealthCenters)] <- 0

MHC_CORR <- data.frame(Variable = character(),
                       Correlation = numeric(),
                       PValue = numeric() )

test <- cor.test(H2AMHC$H2A_workers, H2AMHC$MigrantHealthCenters, method = "spearman", use = "complete.obs", exact = F, conf.level = 0.95)
MHC_CORR <- rbind(MHC_CORR, data.frame(Variable = "MigrantHealthCenters",
                                       Correlation = round(test$estimate, 3),
                                       PValue = round(test$p.value, 3 )))

MHC_CORR$Significance <- ifelse(MHC_CORR$PValue <= 0.05, TRUE, FALSE)
rownames(MHC_CORR) <- NULL
kable(MHC_CORR, caption = "Table 1: Correlation between H-2A Population and Migrant Health Centers")



BURN <- JOIN_CUT |> select("H2A_workers", matches("drought|wildfire|heat|heatwave|Aug|Jul|July"))
#BURN$state <- NULL
BURN_CORR <- data.frame(Variable = character(),
                        Correlation = numeric(),
                        PValue = numeric() )

for (predictor in colnames(BURN)) {
  test <- cor.test(BURN$H2A_workers, BURN[[predictor]], method = "spearman", use = "complete.obs", exact = F, conf.level = 0.95)
  BURN_CORR <- rbind(BURN_CORR, data.frame(Variable = predictor,
                                           Correlation = round(test$estimate, 3),
                                           PValue = round(test$p.value,3 )))
}

BURN_CORR <- BURN_CORR |> arrange(desc(Correlation))
BURN_CORR$Significance <- ifelse(BURN_CORR$PValue <= 0.05, TRUE, FALSE)
BURN_CORR <- BURN_CORR[-1,]
rownames(BURN_CORR) <- NULL
kable(BURN_CORR, caption = "Table 2: Correlation Between H-2A Population and Fire/Heat Environmental Risk")

# Corrplot
COR_MTX <- round(cor(BURN, method = "spearman", use = "complete.obs"), 2)
PMAT <- cor_pmat(COR_MTX)
ggcorrplot(
  COR_MTX,
  p.mat = PMAT,
  method = "square",
  type = "lower",
  outline.color = "lightgrey",
  sig.level = 0.05,
  insig = "blank",
  lab = T,
  lab_size = 2,
  legend.title = "Correlation"
) +
  theme(
    axis.text.x = element_text(size = 6.5, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 6.5)
  ) +
  labs(title = "Figure 4: Correlation Between H-2A Population & Environmental Risk")


TEMP <- as.data.frame(COR_MTX)
COR_MTX2 <- as.matrix(TEMP[,1])
rownames(COR_MTX2) <- rownames(COR_MTX)
ggcorrplot(
  COR_MTX2,
  method = "square",
  type = "lower",
  outline.color = "lightgrey",
  sig.level = 0.05,
  insig = "blank",
  lab = T,
  lab_size = 2,
  legend.title = "Correlation"
) +
  theme(
    axis.text.x = element_text(size = 6.5, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 6.5)
  ) +
  labs(title = "Figure 4.2: Correlation Between H-2A Population & Environmental Risk")




FIRE <- JOIN_CUT |> select(40:length(JOIN_CUT), matches("drought|wildfire|heat|heatwave"))
colnames(FIRE) <- gsub(" ", "", colnames(FIRE))
colnames(FIRE) <- gsub("(^|_)([a-z])", "\\1\\U\\2", colnames(FIRE), perl = T)
colnames(FIRE) <- gsub("\\$", "Dollars", colnames(FIRE))
colnames(FIRE) <- gsub("_", "", colnames(FIRE))


FIRE[] <- lapply(FIRE, as.numeric)
FIRE_HC <- FIRE |> select(3:20)
FIRE_HC <- cbind(FIRE_HC, Population2020 = JOIN$Population2020)
FIRE_HC <- FIRE_HC |>
  mutate(across(
    .cols = -c(MigrantHealthCenters, Population2020),
    .fns = ~ .x / Population2020
  ))
colnames(FIRE_HC)[!colnames(FIRE_HC) %in% c("MigrantHealthWorkers", "Population2020")] <-
  paste0(colnames(FIRE_HC)[!colnames(FIRE_HC) %in% c("MigrantHealthWorkers", "Population2020")], "PerCapita")
FIRE_HC <- FIRE_HC |> select(-Population2020)

FIRE_PRED <- FIRE |> select(21:length(FIRE))

FIRE_CORR <- data.frame(Response = character(),
                        Predictor = character(),
                        Correlation = numeric(),
                        PValue = numeric() )

for (response in colnames(FIRE_HC)) {
  for( predictor in colnames(FIRE_PRED)) {
    FIRE_test <- cor.test(FIRE_HC[[response]], FIRE_PRED[[predictor]], method = "spearman", use = "complete.obs", exact = F, conf.level = 0.95)
    FIRE_CORR <- rbind(FIRE_CORR, data.frame(Response = response, Predictor = predictor, Correlation = round(FIRE_test$estimate, 3), PValue = round(FIRE_test$p.value, 3)))
  }
}

FIRE_CORR <- FIRE_CORR |> arrange(desc(Correlation))
FIRE_CORR$Significance <- ifelse(FIRE_CORR$PValue <= 0.05, TRUE, FALSE)
rownames(FIRE_CORR) <- NULL

kable(FIRE_CORR, caption = "Table 3: Correlation between healthcare facilities and Fire/Heat Environmental Risk")



BLAZE_PRED <- JOIN_CUT |> select(matches("drought|wildfire|heat|heatwave"))
BLAZE_AGO <- JOIN_CUT |> select(intersect(colnames(AGO), colnames(JOIN_CUT))) |> select(-state, -county)
BLAZE_AGO[] <- lapply(BLAZE_AGO, as.numeric)
BLAZE_AGO[is.na(BLAZE_AGO)] <- 0
BLAZE <- cbind(BLAZE_PRED, BLAZE_AGO)

colnames(BLAZE) <- gsub("_", " ", colnames(BLAZE))
colnames(BLAZE) <- gsub("\\b([a-z])", "\\U\\1", colnames(BLAZE), perl = TRUE)
colnames(BLAZE) <- gsub(" ", "", colnames(BLAZE))
colnames(BLAZE) <- gsub("\\$", "Dollars", colnames(BLAZE))

colnames(BLAZE_AGO) <- gsub("_", " ", colnames(BLAZE_AGO))
colnames(BLAZE_AGO) <- gsub("\\b([a-z])", "\\U\\1", colnames(BLAZE_AGO), perl = TRUE)
colnames(BLAZE_AGO) <- gsub(" ", "", colnames(BLAZE_AGO))
colnames(BLAZE_AGO) <- gsub("\\$", "Dollars", colnames(BLAZE_AGO))

colnames(BLAZE_PRED) <- gsub("_", " ", colnames(BLAZE_PRED))
colnames(BLAZE_PRED) <- gsub("\\b([a-z])", "\\U\\1", colnames(BLAZE_PRED), perl = TRUE)
colnames(BLAZE_PRED) <- gsub(" ", "", colnames(BLAZE_PRED))
colnames(BLAZE_PRED) <- gsub("\\$", "Dollars", colnames(BLAZE_PRED))

BLAZE_AGO <- BLAZE_AGO |> select(-JOINCUTDollarsLatitude, -JOINCUTDollarsLongitude)

response <- character()
predictor <- character()

BLAZE_CORR <- data.frame(Predictor = character(),
                         Response = character(),
                         Correlation = numeric(),
                         PValue = numeric())

for (response in colnames(BLAZE_AGO)) {
  for (predictor in colnames(BLAZE_PRED)) {
    BLAZE_test <- cor.test(BLAZE[[response]], BLAZE[[predictor]], method = "spearman", use = "complete.obs", exact = F, conf.level = 0.95)
    BLAZE_CORR <- rbind(BLAZE_CORR, data.frame(Predictor = predictor, Response = response, Correlation = round(BLAZE_test$estimate, 3), PValue = round(BLAZE_test$p.value, 3)))
  }
}

BLAZE_CORR <- BLAZE_CORR |> arrange(desc(Correlation))
BLAZE_CORR$Significance <- ifelse(BLAZE_CORR$PValue <= 0.05, TRUE, FALSE)
rownames(BLAZE_CORR) <- NULL
BLAZE <- BLAZE |> select(-JOINCUTDollarsLatitude, -JOINCUTDollarsLongitude)
kable(BLAZE_CORR, caption = "Table 4: Correlation between Farm output and Wildfire/Drought/HeatWave Exposure")

# Corrplot
COR_MTX <- round(cor(BLAZE, method = "spearman", use = "complete.obs"), 2)
PMAT <- cor_pmat(COR_MTX)
ggcorrplot(
  COR_MTX,
  p.mat = PMAT,
  method = "square",
  type = "lower",
  outline.color = "lightgray",
  sig.level = 0.05,
  insig = "blank",
  lab = T,
  lab_size = 1.34,
  legend.title = "Correlation"
) +
  theme(
    axis.text.x = element_text(size = 6.5, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 6.5)
  ) +
  labs(title = "Figure 5: Correlation between Farm Output & Environmental Risk")
#===============================================================================================#

COLORADO_MHC <- JOIN_CUT |> filter(state=="Colorado") |> arrange(desc(H2A_workers))
cx <- cor.test(COLORADO_MHC$H2A_workers, COLORADO_MHC$MigrantHealthCenters)

UTAH_MHC <- JOIN_CUT |> filter(state=="Utah") |> arrange(desc(H2A_workers))
ux <- cor.test(UTAH_MHC$H2A_workers, UTAH_MHC$MigrantHealthCenters)

NORTHDAKOTA_MHC <- JOIN_CUT |> filter(state == "North Dakota") |> arrange(desc(H2A_workers))
nx <- cor.test(NORTHDAKOTA_MHC$H2A_workers, NORTHDAKOTA_MHC$MigrantHealthCenters)

MONTANA_MHC <- JOIN_CUT |> filter(state=="Montana") |> arrange(desc(H2A_workers))
mx <- cor.test(MONTANA_MHC$H2A_workers, MONTANA_MHC$MigrantHealthCenters)

STATE_MHC_CORR <- data.frame(State = character(),
                             Correlation = numeric(),
                             P.Value = numeric(),
                             Significant = logical())

STATE_MHC_CORR <- rbind(STATE_MHC_CORR,
                        data.frame(State = "Colorado", Correlation = cx$estimate,
                                   P.Value = cx$p.value, Significant = NA) )

STATE_MHC_CORR <- rbind(STATE_MHC_CORR,
                        data.frame(State = "Utah", Correlation = ux$estimate,
                                   P.Value = ux$p.value, Significant = NA) )

STATE_MHC_CORR <- rbind(STATE_MHC_CORR,
                        data.frame(State = "North Dakota", Correlation = nx$estimate,
                                   P.Value = nx$p.value, Significant = NA) )

STATE_MHC_CORR <- rbind(STATE_MHC_CORR,
                        data.frame(State = "Montana", Correlation = mx$estimate,
                                   P.Value = mx$p.value, Significant = NA) )

STATE_MHC_CORR <- rbind(STATE_MHC_CORR, data.frame(State = "South Dakota", Correlation = NA, P.Value = NA, Significant = NA))
STATE_MHC_CORR <- rbind(STATE_MHC_CORR, data.frame(State = "Wyoming", Correlation = NA, P.Value = NA, Significant = NA))

STATE_MHC_CORR$Significant <- STATE_MHC_CORR$P.Value <= 0.05

STATE_MHC_CORR$Correlation <- round(STATE_MHC_CORR$Correlation, 3)
STATE_MHC_CORR$P.Value <- round(STATE_MHC_CORR$P.Value, 3)

rownames(STATE_MHC_CORR) <- NULL

STATE_MHC_CORR <- STATE_MHC_CORR |> arrange(desc(Correlation))
STATE_MHC_CORR




cor.test(COLORADO_MHC$H2A_workers, COLORADO_MHC$AgricultureValue)

cor.test(UTAH_MHC$H2A_workers, UTAH_MHC$AgricultureValue)

WYOMING_MHC <- JOIN_CUT |> filter(state=="Wyoming") |> arrange(desc(H2A_workers))
cor.test(WYOMING_MHC$H2A_workers, WYOMING_MHC$AgricultureValue)

cor.test(NORTHDAKOTA_MHC$H2A_workers, NORTHDAKOTA_MHC$AgricultureValue)

cor.test(MONTANA_MHC$H2A_workers, MONTANA_MHC$AgricultureValue)

SOUTHDAKOTA_MHC <- JOIN_CUT |> filter(state=="South Dakota") |> arrange(desc(H2A_workers))
cor.test(SOUTHDAKOTA_MHC$H2A_workers, SOUTHDAKOTA_MHC$AgricultureValue)
