generate_corr_plot <- function(df_names) {
  for (df_name in df_names) {

    df <- get(df_name, envir = .GlobalEnv)
    corr <- round(cor(df, method = "spearman"), 2)
    matrix <- cor_pmat(df)
    corrplot <- ggcorrplot(corr, p.mat = matrix, method = "square", type = "full",
                           lab = TRUE, lab_size = 1, insig = "blank", title = paste("Correlation Plot:", df_name)) +
      theme(axis.text.x = element_text(size = 4, angle = 90, hjust = 1),
            axis.text.y = element_text(size = 4))

    assign(paste(df_name, "_corrplot", sep = ""), corrplot, envir = .GlobalEnv)
  }
}

N <- c("drought", "wildfire", "Cold.Wave", "tornado", "ice.storm", "winter.weather", "strong.wind", "hail", "heat.wave", "avalanche", "Landslide", "lightning")
# Example usage with a list of data frame names
generate_corr_plot(N)
