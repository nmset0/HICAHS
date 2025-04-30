# Function to read in 289 data sets
# !! Used to compile agricultural census data
# No Longer Necessary

library(data.table)

load_ag_output_data <- function(base_path, states) {
  state_outputs <- list()

  for (state in states) {
    state_path <- file.path(base_path, state) # Path to files

    if (dir.exists(state_path)) {
      setwd(state_path)  # Change working directory to that of the files

      files <- list.files(pattern = "*.csv")

      if (length(files) > 0) {
        dataset <- do.call(rbind, lapply(files, fread))
        state_outputs[[state]] <- as.data.frame(unclass(dataset))
      }
    }
  }
  # binding all data
  ag_output <- do.call(rbind, state_outputs)

  # Return to original working directory
  setwd(dirname(base_path))

  return(ag_output)
}

# base_path = working directory
# states = CO, WY, UT, MT, ND, SD