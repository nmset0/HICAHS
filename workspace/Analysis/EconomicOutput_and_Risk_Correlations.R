rm(list = ls())
library(tidyverse)
library(dplyr)
library(ggcorrplot)

output_risk_combined <- readr::read_csv("workspace/output_risk_combined.csv")

problematic_cols <- sapply(output_risk_combined, function(x) {
  is.character(x) && any(grepl("\\d+", x))
})

print("Columns that should be numeric but are character:")
for(col in names(output_risk_combined)[problematic_cols]) {
  cat("\nColumn:", col, "\n")
  cat("First few values:", head(output_risk_combined[[col]]), "\n")
  cat("Class:", class(output_risk_combined[[col]]), "\n")
}

output_risk_combined <- output_risk_combined |>
  mutate(across(where(is.character), ~{
    if(any(grepl("\\d+", .x))) {
      cleaned <- gsub("[^0-9.-]", "", .x)
      as.numeric(cleaned)
    } else {
      .x
    }
  }))


# Correlating crop_totals_sales_measured_in_$ with all other variables
output_risk_combined_num <- output_risk_combined[, sapply(output_risk_combined, is.numeric)]
N <- colnames(output_risk_combined_num)

correlation_results <- data.frame(column = character(), correlation = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

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

correlation_df <- correlation_results |>
  arrange(desc(correlation)) |>
  filter(!is.na(correlation))

correlation_df <- correlation_df |>
  mutate(category = case_when(
    grepl("drought|wildfire|Cold.Wave|tornado|ice.storm|winter.weather|strong.wind|hail|heat.wave|avalanche|Landslide|lightning", column, ignore.case = TRUE) ~ "nature",
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

knitr::kable(correlation_df)
#view(correlation_df)

# See 'outputriskfunction.R' for final correlation analysis method

# split_dfs <- split(correlation_df, correlation_df$category)
# list2env(split(correlation_df, correlation_df$category), envir = .GlobalEnv)




output_environmentalRisk <- output_risk_combined  |>
  select(matches("farm_sales_|drought|wildfire|flood|Cold\\.Wave|tornado|ice\\.storm|winter\\.weather|strong\\.wind|hail|heat\\.wave|avalanche|Landslide|lightning|income")) |>
  select(where(is.numeric))
output_environmentalRisk[is.na(output_environmentalRisk)] <- 0
subset_df <- output_environmentalRisk[, grep("income|sales|farm_sales", names(output_environmentalRisk), ignore.case = TRUE), drop = FALSE]


risk_types <- c("drought", "wildfire", "Cold\\.Wave", "tornado",
                "ice\\.storm", "winter\\.weather", "strong\\.wind", "hail",
                "heat\\.wave", "avalanche", "Landslide", "lightning")

risk_dfs <- list()

for (risk in risk_types) {
  clean_risk_name <- gsub("\\\\", "", risk)

  risk_dfs[[clean_risk_name]] <- output_environmentalRisk[, grepl(risk, names(output_environmentalRisk), ignore.case = TRUE)] |>
    cbind(subset_df)
}

for (i in names(risk_dfs)) {
  assign(i, risk_dfs[[i]])
}


# See create_correlogram_function.R
# generate_corr_plot <- function(df_names) {
#
#   for (df_name in df_names) {
#     df <- get(df_name, envir = .GlobalEnv)
#     corr <- round(cor(df, method = "spearman"), 2)
#     matrix <- cor_pmat(df)
#     corrplot <- ggcorrplot(corr, p.mat = matrix, method = "square", type = "lower",
#                            lab = TRUE, lab_size = 1, insig = "blank", title = paste("Correlation Plot:", df_name)) +
#       theme(axis.text.x = element_text(size = 4, angle = 55, hjust = 1),
#             axis.text.y = element_text(size = 4))
#
#     assign(paste(df_name, "_corrplot", sep = ""), corrplot, envir = .GlobalEnv)
#   }
# }

# N <- c("drought", "wildfire", "flood", "Cold.Wave", "tornado", "ice.storm", "winter.weather", "strong.wind", "hail", "heat.wave", "avalanche", "Landslide", "lightning")
# # Example usage with a list of data frame names
# generate_corr_plot(N)



