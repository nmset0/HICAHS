library(ggcorrplot)
library(tidyverse)

output_risk_combined <- readr::read_csv("workspace/output_risk_combined.csv")

output_risk_combined_num[] <- lapply(output_risk_combined_num, function(x) {
  if (any(grepl("\\d+", x)) & !is.numeric(x)) {
    as.numeric(x)
  } else {
    x
  }
})



correlation_vector <- c()
output_risk_combined_num <- output_risk_combined[, sapply(output_risk_combined, is.numeric)]
N <- colnames(output_risk_combined_num)
for (i in N) {
  correlation_value <- cor(x = output_risk_combined_num[[i]], y = output_risk_combined_num$`crop_totals_sales_measured_in_$`, method = "spearman")
  correlation_vector <- c(correlation_vector, correlation_value)
}
correlation_df <- data.frame(column = N, correlation = correlation_vector) |> arrange(desc(correlation))
correlation_df <-  correlation_df[-1, ]
knitr::kable(correlation_df)
