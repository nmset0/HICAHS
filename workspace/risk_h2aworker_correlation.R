library(tidyverse)
library(ggcorrplot)

rm(list = ls())

Risk_H2AWorkers <- read_csv("~/internship/workspace/wildfire_disaster.csv")


# Correlation between heat and worker concentrations
MaxTemp_WorkerTotal_lm <- lm(total_workers ~ maximum_temperature, data = Risk_H2AWorkers)
summary(MaxTemp_WorkerTotal_lm)$coefficients

# Correlation between fire and worker concentrations
FireHzrdIdxScore_WorkerTotal_lm <- lm(total_workers ~ `wildfire_hazard_type_risk_index_score`, data = Risk_H2AWorkers)
FireExpTotal_WorkerTotal_lm <- lm(total_workers ~ `wildfire_exposure_total`, data = Risk_H2AWorkers)

summary(FireHzrdIdxScore_WorkerTotal_lm)$coefficients
summary(FireExpTotal_WorkerTotal_lm)$coefficients # SIGNIFICANCE


# Separating by state
split_datasets <- split(Risk_H2AWorkers, Risk_H2AWorkers$state)

colorado <- split_datasets[[1]]
  montana <- split_datasets[[2]]
    north_dakota <- split_datasets[[3]]
      south_dakota <- split_datasets[[4]]
        utah <- split_datasets[[5]]
          wyoming <- split_datasets[[6]]

# Linear Models
colorado_lm <- lm(maximum_temperature ~ total_workers, data = colorado)
  wyoming_lm <- lm(maximum_temperature ~ total_workers, data = wyoming)
    north_dakota_lm <- lm(maximum_temperature ~ total_workers, data = north_dakota)
      south_dakota_lm <- lm(maximum_temperature ~ total_workers, data = south_dakota)
        utah_lm <- lm(maximum_temperature ~ total_workers, data = utah)

summary(colorado_lm)
  summary(wyoming_lm)
    summary(north_dakota_lm)
      summary(south_dakota_lm)
        summary(utah_lm)

# Pearson Correlation Coefficient
# complemented by lm() models
# values from cor()^2 = `Multiple R-squared`
CO_rsq <- cor(x = colorado$maximum_temperature, y = colorado$total_workers)
  MT_rsq <- cor(x = montana$maximum_temperature, y = montana$total_workers)
    ND_rsq <- cor(x = north_dakota$maximum_temperature, y = north_dakota$total_workers)
      SD_rsq <- cor(x = south_dakota$maximum_temperature, y = south_dakota$total_workers)
        WY_rsq <- cor(x = wyoming$maximum_temperature, y = wyoming$total_workers)
          UT_rsq <- cor(x = utah$maximum_temperature, y = utah$total_workers)

# tibble(CO_rsq, MT_rsq, ND_rsq, SD_rsq, WY_rsq)


r_squared <- data.frame(State = c("Colorado", "Wyoming", "North Dakota", "South Dakota", "Utah"),
                          Model_R_sqr = c(
                            summary(colorado_lm)$r.squared,
                            summary(wyoming_lm)$r.squared,
                            summary(north_dakota_lm)$r.squared,
                            summary(south_dakota_lm)$r.squared,
                            summary(utah_lm)$r.squared),
                        Pearson_Corr = c(CO_rsq, WY_rsq, ND_rsq, SD_rsq, UT_rsq))

tibble(r_squared)

# Correlation between all variables and worker density
correlation_vector <- c()
  Risk_H2AWorkers_num <- Risk_H2AWorkers[, sapply(Risk_H2AWorkers, is.numeric)]
    N <- colnames(Risk_H2AWorkers_num)
      for (i in N) {
        correlation_value <- cor(x = Risk_H2AWorkers_num[[i]], y = Risk_H2AWorkers_num$total_workers, method = "spearman")
        correlation_vector <- c(correlation_vector, correlation_value)
      }
        correlation_df <- data.frame(column = N, correlation = correlation_vector) |> arrange(desc(correlation))
          correlation_df <-  correlation_df[-1, ]
            knitr::kable(correlation_df)

# Correlogram ggcorrplot()
temp <- Risk_H2AWorkers[sapply(Risk_H2AWorkers, is.numeric)]
corr <- round(cor(temp, method = "spearman"),3)
matrix <- cor_pmat(temp)
corrplot <- ggcorrplot(corr, p.mat = matrix, type = "full",
    lab = TRUE, lab_size = 1.6, insig = "blank", title = "Correlation Plot") +
    theme(axis.text.x = element_text(size = 4.1, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 4.1)
)
corrplot

# write_csv(correlation_df, file = "Risk_Correlation_df.csv")
# ggsave(filename = "Risk_Corr_Plot.png", plot = corrplot, width = 10, height = 10, units = "in")

