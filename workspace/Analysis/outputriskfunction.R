process_risk_data <- function(file_path) {
  library(readr)
  library(dplyr)
  library(knitr)

  # Read the dataset
  output_risk_combined <- read_csv(file_path)

  # Identify problematic character columns containing numeric values
  problematic_cols <- sapply(output_risk_combined, function(x) {
    is.character(x) && any(grepl("\\d+", x))
  })

  print("Columns that should be numeric but are character:")
  for (col in names(output_risk_combined)[problematic_cols]) {
    cat("\nColumn:", col, "\n")
    cat("First few values:", head(output_risk_combined[[col]]), "\n")
    cat("Class:", class(output_risk_combined[[col]]), "\n")
  }

  # Convert problematic character columns to numeric
  output_risk_combined <- output_risk_combined |>
    mutate(across(where(is.character), ~{
      if (any(grepl("\\d+", .x))) {
        cleaned <- gsub("[^0-9.-]", "", .x)
        as.numeric(cleaned)
      } else {
        .x
      }
    }))

  # Extract numeric columns
  output_risk_combined_num <- output_risk_combined[, sapply(output_risk_combined, is.numeric)]
  N <- colnames(output_risk_combined_num)

  # Initialize correlation results
  correlation_results <- data.frame(column = character(), correlation = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

  # Compute correlations with crop_totals_sales_measured_in_$
  for (i in seq_along(N)) {
    if (N[i] != "crop_totals_sales_measured_in_$") {
      test_result <- cor.test(
        x = output_risk_combined_num[[i]],
        y = output_risk_combined_num$`crop_totals_sales_measured_in_$`,
        method = "spearman",
        use = "complete.obs"
      )

      correlation_results <- rbind(correlation_results, data.frame(
        column = N[i],
        correlation = test_result$estimate,
        p_value = test_result$p.value
      ))
    }
  }

  # Process correlation results
  correlation_df <- correlation_results |>
    arrange(desc(correlation)) |>
    filter(!is.na(correlation)) |>
    mutate(category = case_when(
      grepl("drought|wildfire|flood|Cold.Wave|tornado|ice.storm|winter.weather|strong.wind|hail|heat.wave|avalanche|Landslide|lightning", column, ignore.case = TRUE) ~ "nature",
      grepl("sweet_potatoes|sunflower|sorghum|wheat|oats|hay_|corn|sugarbeets|potatoes_|barley|beans|soybean|vegetable|orchards", column, ignore.case = TRUE) ~ "crops",
      grepl("community|social|Social.Vulnerability.and.", column, ignore.case = TRUE) ~ "social",
      grepl("sheep|chicken|hogs|cattle|animal", column, ignore.case = TRUE) ~ "animals",
      grepl("area_operated", column, ignore.case = TRUE) ~ "area_operated",
      grepl("Expected.", column, ignore.case = TRUE) ~ "expectedLoss",
      grepl("national.risk.index", column, ignore.case = TRUE) ~ "riskIndex",
      grepl("farm_operations", column, ignore.case = TRUE) ~ "operations",
      grepl("income", column, ignore.case = TRUE) ~ "income",
      grepl("expense", column, ignore.case = TRUE) ~ "costs",
      grepl("ag_land", column, ignore.case = TRUE) ~ "agriculturalLand",
      grepl("farm_sales_", column, ignore.case = TRUE) ~ "salesQuantity",
      TRUE ~ "other"
    )) |>
    arrange(category, p_value) |>
    rename(predictor = column)

  # Display correlation table
  print(kable(correlation_df))
  View(correlation_df)

  # Split data frames by category and return as a list
  return(correlation_df)
}