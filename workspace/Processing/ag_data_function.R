library(data.table)

load_ag_output_data <- function(base_path, states) {
  state_outputs <- list()

  for (state in states) {
    state_path <- file.path(base_path, state)

    if (dir.exists(state_path)) {
      setwd(state_path)  # Change working directory

      files <- list.files(pattern = "*.csv")

      if (length(files) > 0) {
        dataset <- do.call(rbind, lapply(files, fread))
        state_outputs[[state]] <- as.data.frame(unclass(dataset))
      }
    }
  }

  ag_output <- do.call(rbind, state_outputs)

  # Reset working directory (Optional)
  setwd(dirname(base_path))

  return(ag_output)
}