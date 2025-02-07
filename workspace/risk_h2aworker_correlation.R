library(tidyverse)
library(ggcorrplot)

rm(list = ls())

Risk_H2AWorkers <- read_csv("workspace/Risk_H2AWorkers.csv")

# Correlation between heat and worker concentrations
# x = max temp. per county, y = number of workers in county
MaxTemp_WorkerTotal_lm <- lm(total_workers_h2a ~ max_temp, data = Risk_H2AWorkers)
summary(MaxTemp_WorkerTotal_lm)$coefficients
# No correlation? Reasonable answer, but unsure if this is desired model

# Correlation between heat and worker density

# Pearson Correlation Coefficient
cor(x = Risk_H2AWorkers$max_temp, y = Risk_H2AWorkers$total_workers_h2a)

# Separating by state
split_datasets <- split(Risk_H2AWorkers, Risk_H2AWorkers$state)

colorado <- split_datasets[[1]]
  montana <- split_datasets[[2]]
    north_dakota <- split_datasets[[3]]
      south_dakota <- split_datasets[[4]]
        utah <- split_datasets[[5]]
          wyoming <- split_datasets[[6]]

# Linear Models
colorado_lm <- lm(max_temp ~ total_workers_h2a, data = colorado)
  wyoming_lm <- lm(max_temp ~ total_workers_h2a, data = wyoming)
    north_dakota_lm <- lm(max_temp ~ total_workers_h2a, data = north_dakota)
      south_dakota_lm <- lm(max_temp ~ total_workers_h2a, data = south_dakota)
        utah_lm <- lm(max_temp ~ total_workers_h2a, data = utah)

summary(colorado_lm)
  summary(wyoming_lm)
    summary(north_dakota_lm)
      summary(south_dakota_lm)
        summary(utah_lm)

# Pearson Correlation Coefficient
# complemented by lm() models
# values from cor()^2 = `Multiple R-squared`
CO_rsq <- cor(x = colorado$max_temp, y = colorado$total_workers_h2a)
  MT_rsq <- cor(x = montana$max_temp, y = montana$total_workers_h2a)
    ND_rsq <- cor(x = north_dakota$max_temp, y = north_dakota$total_workers_h2a)
      SD_rsq <- cor(x = south_dakota$max_temp, y = south_dakota$total_workers_h2a)
        WY_rsq <- cor(x = wyoming$max_temp, y = wyoming$total_workers_h2a)
          UT_rsq <- cor(x = utah$max_temp, y = utah$total_workers_h2a)

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

# Correlogram ggcorrplot()
wildfire <- read_csv("~/internship/workspace/wildfire_disaster.csv")

temp <- wildfire[sapply(wildfire, is.numeric)]
corr <- round(cor(temp),2)
matrix <- cor_pmat(temp)
plot <- ggcorrplot(corr, p.mat = matrix, type = "full",
    lab = TRUE, lab_size = 1.6, insig = "blank", title = "Correlation Plot") +
    theme(axis.text.x = element_text(size = 4.1, angle = 90, hjust = 1),
    axis.text.y = element_text(size = 4.1)
)

# ggsave(filename = "Risk_Corr_Plot.png", plot = plot, width = 10, height = 10, units = "in")

