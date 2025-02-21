create_correlograms <- function(temp, chunk_size = 30, output_dir = "correlation_plots") {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }

  # Get the total number of variables (columns)
  num_vars <- ncol(temp)

  # Loop through chunks to create correlograms
  for (i in 1:ceiling(num_vars / chunk_size)) {
    # Randomly select 30 columns (or the remaining ones if fewer than 30)
    selected_columns <- sample(num_vars, min(chunk_size, num_vars - (i - 1) * chunk_size))

    # Get the chunk of the dataframe
    chunk <- temp[, selected_columns]

    # Compute Spearman correlation matrix
    corr <- round(cor(chunk, method = "spearman"), 3)

    # Compute p-value matrix for the chunk
    matrix <- cor_pmat(chunk)

    # Generate correlogram for this chunk with numbers in the boxes and axis labels
    corrplot <- ggcorrplot(corr, p.mat = matrix, type = "full",
                           lab = TRUE, lab_size = 1, insig = "blank", title = paste("Correlation Plot - Chunk", i)) +
      theme(
        axis.text.x = element_text(size = 3, angle = 90, hjust = 1),  # Adjust x-axis label size and angle
        axis.text.y = element_text(size = 3),  # Adjust y-axis label size
        axis.ticks = element_line(),         # Keep axis ticks
        axis.title.x = element_text(size = 8), # Adjust x-axis title size
        axis.title.y = element_text(size = 8)  # Adjust y-axis title size
      )

    # Save the plot as a .png file
    ggsave(filename = file.path(output_dir, paste("correlation_plot_chunk_", i, ".png", sep = "")),
           plot = corrplot, width = 8, height = 6)
  }
}
