library(tidyverse)
library(readr)
library(data.table)

rm(list=ls())

# Importing data (290 datasets)
disaster <- read_csv("~/internship/workspace/wildfire_disaster.csv")

setwd("C:/Users/natha/OneDrive/Documents/internship/workspace/Ag Output Data/Colorado")
  files = list.files(pattern="*.csv")
    dataset = do.call(rbind, lapply(files, fread))
      rm(files)
        coloradoOutput <- as.data.frame(unclass(dataset))
setwd("C:/Users/natha/OneDrive/Documents/internship/workspace/Ag Output Data/Wyoming")
  files = list.files(pattern="*.csv")
    dataset = do.call(rbind, lapply(files, fread))
      rm(files)
        wyomingOutput <- as.data.frame(unclass(dataset))
setwd("C:/Users/natha/OneDrive/Documents/internship/workspace/Ag Output Data/North_Dakota")
  files = list.files(pattern="*.csv")
    dataset = do.call(rbind, lapply(files, fread))
      rm(files)
        northDakotaOutput <- as.data.frame(unclass(dataset))
setwd("C:/Users/natha/OneDrive/Documents/internship/workspace/Ag Output Data/South_Dakota")
  files = list.files(pattern="*.csv")
    dataset = do.call(rbind, lapply(files, fread))
      rm(files)
        southDakotaOutput <- as.data.frame(unclass(dataset))
setwd("C:/Users/natha/OneDrive/Documents/internship/workspace/Ag Output Data/Utah")
  files = list.files(pattern="*.csv")
    dataset = do.call(rbind, lapply(files, fread))
      rm(files)
        utahOutput <- as.data.frame(unclass(dataset))
setwd("C:/Users/natha/OneDrive/Documents/internship/workspace/Ag Output Data/Montana")
    files = list.files(pattern="*.csv")
      dataset = do.call(rbind, lapply(files, fread))
        rm(files)
          montanaOutput <- as.data.frame(unclass(dataset))

setwd("C:/Users/natha/OneDrive/Documents/internship")
rm(dataset)

ag_output <- rbind(coloradoOutput, northDakotaOutput, southDakotaOutput, montanaOutput, utahOutput, wyomingOutput)

for (i in 1:nrow(ag_output)) {
  if (!is.na(ag_output$domain.category[i]) && ag_output$domain.category[i] != "") {
    ag_output$data.item[i] <- ag_output$domain.category[i]
  }
}

ag_output <- ag_output %>% select(-domain.category, -state.fips, -commodity, -county.code)
ag_output <- as.data.frame(lapply(ag_output, function(x) {
  if (is.character(x)) tolower(x) else x
}))


ag_output_wide <- ag_output %>%
  pivot_wider(names_from = data.item, values_from = value)

colnames(ag_output_wide) <- gsub(":", "", colnames(ag_output_wide))
  colnames(ag_output_wide) <- gsub("-", "", colnames(ag_output_wide))
    colnames(ag_output_wide) <- gsub(",", "", colnames(ag_output_wide))
      colnames(ag_output_wide) <- gsub("/", "", colnames(ag_output_wide))
        colnames(ag_output_wide) <- gsub(" ", "_", colnames(ag_output_wide))
          colnames(ag_output_wide) <- gsub("__", "_", colnames(ag_output_wide))

view(ag_output_wide)
