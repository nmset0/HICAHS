rm(list = ls())
library(randomForest)
library(reprtree)
library(caret)
source("~/internship/workspace/Processing/Healthcare.R")

migrant_health_centers()
state_facility_load()
healthcare()
stateFacilityCorrelations()
weather_facility_load()
data_for_feature_selection()

# Random Forest model for feature selection
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

  predictor_variables <- setdiff(names(feature_selection_data), response_variable)

  # Convert the dataset to a formula
  formula <- as.formula(paste(response_variable, "~", paste(predictor_variables, collapse = " + ")))

  # cv tuning
  control <- trainControl(method = "cv", number = 10)
  tuned_rf <- train(formula, data = feature_selection_data, method = "rf",
                    trControl = control,
                    tuneLength = tuningLength)  # Try different values of mtry

  #print(tuned_rf$bestTune)

  # Fit random forest model
  rf_model <- randomForest(formula, data = feature_selection_data,
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

niter <- 10 # Whatever I want it to be

start.time <- Sys.time()
for (i in 1:niter) {
  random_forest_model("Hospitals", 10, 1000)
  cat("Iteration: ", i)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
