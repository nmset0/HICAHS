rm(list = ls())
source("~/internship/workspace/Processing/Healthcare.R")
migrant_health_centers()
state_facility_load()
healthcare()
stateFacilityCorrelations()
weather_facility_load()

library(randomForest)
library(tree)
library(reprtree)
library(caret)
library(ggplot2)

colnames(colorado_full) <- gsub("\\(\\$\\)", "", colnames(colorado_full))
colnames(colorado_full) <- gsub("[()]", "", colnames(colorado_full))


exposure_columns <- grep("ExposureTotal", colnames(colorado_full), value = TRUE)
exposure_columns_formula <- paste(exposure_columns, collapse = " + ")
model_formula <- as.formula(paste("migranthealthcenters ~ AgricultureValue + NationalRiskIndexValueComposite + ", exposure_columns_formula))
model <- glm(model_formula, data = colorado_full[,-1])

summary(model)



# Fit a random forest model
rf_model <- randomForest(Hospitals ~ ., data = colorado_full)
importance_values <- importance(rf_model)

ordered_importance <- importance_values[order(-importance_values[, 1]), ]
importance_df <- data.frame(Variable = row.names(importance_values), Importance = importance_values[, 1])
print(importance_df[2:10,])

varImpPlot(rf_model)

model <- glm(Hospitals ~ BuildingValue + IceStormExposureTotal +
               IceStormExposurePopulation +
               WinterWeatherExposureBuildingValue +
               WinterWeatherExposureTotal +
               IceStormExposurePopulationEquivalence +
               ColdWaveExposureBuildingValue +
               IceStormExposureBuildingValue +
               Population2020,
             data = colorado_full)


summary(model)

###

random_forest_model <- function(response_variable, tuningLength, numTrees) {
  # Check parameter classes
  if (!is.character(response_variable)) {
    stop("FAIL")
  }
  if (!is.numeric(tuningLength) && !is.integer(tuningLength)) {
    stop("FAIL")
  }
  if (!is.numeric(numTrees) && !is.integer(numTrees)) {
    stop("FAIL")
  }

  predictor_variables <- setdiff(names(colorado_full), response_variable)

  # Convert the dataset to a formula
  formula <- as.formula(paste(response_variable, "~", paste(predictor_variables, collapse = " + ")))

  # cv tuning
  control <- trainControl(method = "cv", number = 10)
  tuned_rf <- train(formula, data = colorado_full, method = "rf",
                    trControl = control,
                    tuneLength = tuningLength)  # Try different values of mtry

  #print(tuned_rf$bestTune)

  # Fit random forest model
  rf_model <- randomForest(formula, data = colorado_full,
                           ntree = numTrees,  # Increase the number of trees
                           mtry = tuned_rf$bestTune$mtry,
                           importance = TRUE)

  # Evaluate predictor importance
  importance_values <- importance(rf_model)
  importance_df <- data.frame(Variable = row.names(importance_values), Importance = importance_values[, 1])
  print(importance_df)

  ordered_importance <- importance_df[order(-importance_df$Importance), ]
  print(ordered_importance)
  varimpplot <- varImpPlot(rf_model)
}


random_forest_model("Hospitals", 15, 1000)
random_forest_model("Hospitals", 15, 1000)
random_forest_model("Hospitals", 15, 1000)
random_forest_model("Hospitals", 15, 1000)
random_forest_model("Hospitals", 15, 1000)
random_forest_model("Hospitals", 15, 1000)
random_forest_model("Hospitals", 15, 1000)
random_forest_model("Hospitals", 15, 1000)
random_forest_model("Hospitals", 15, 1000)
random_forest_model("Hospitals", 15, 1000)



