rm(list = ls())
library(ggcorrplot)
library(tidyverse)

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

correlation_vector <- numeric(length = ncol(output_risk_combined_num))
names(correlation_vector) <- N

for (i in seq_along(N)) {
  correlation_value <- cor(
    x = output_risk_combined_num[[i]],
    y = output_risk_combined_num$`crop_totals_sales_measured_in_$`,
    method = "spearman",
    use = "pairwise.complete.obs"
  )
  correlation_vector[i] <- correlation_value
}

correlation_df <- data.frame(
  column = N,
  correlation = correlation_vector
) |>
  arrange(desc(correlation)) |>
  filter(!is.na(correlation)) |>
  filter(column != "crop_totals_sales_measured_in_$")

knitr::kable(correlation_df)

correlation_df <- correlation_df %>%
  mutate(category = case_when(
    grepl("drought|wildfire|flood|Cold.Wave|tornado|ice.storm|winter.weather|strong.wind|hail|heat.wave|avalanche|Landslide|lightning", column, ignore.case = TRUE) ~ "naturalDisaster",
    grepl("sweet_potatoes|sunflower|sorghum|wheat|oats|hay_|corn|sugarbeets|potatoes_|barley|beans|soybean|vegetable|orchards", column, ignore.case = TRUE) ~ "crops",
    grepl("sheep|chicken|hogs|cattle|animal", column, ignore.case = TRUE) ~ "animals",
    grepl("area_operated", column, ignore.case = TRUE) ~ "area_operated",
    grepl("Expected.", column, ignore.case = TRUE) ~ "expectedLoss",
    grepl("national.risk.index", column, ignore.case = TRUE) ~ "riskIndex",
    grepl("farm_operations", column, ignore.case = TRUE) ~ "operations",
    grepl("community|social", column, ignore.case = TRUE) ~ "social",
    grepl("income|sales|farm_sales", column, ignore.case = TRUE) ~ "income",
    grepl("expense", column, ignore.case = TRUE) ~ "costs",
    grepl("ag_land", column, ignore.case = TRUE) ~ "agriculturalLand",
    TRUE ~ "other"
  )) |>
  arrange(category)


split_dfs <- split(correlation_df, correlation_df$category)
list2env(split(correlation_df, correlation_df$category), envir = .GlobalEnv)

