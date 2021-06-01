
# Housekeeping ------------------------------------------------------------

library(data.table)

# Concatenate all DFs -----------------------------------------------------

# First put all file names about incubators' content into a single list 
all_files <- list.files(pattern = "content.csv", recursive = T)

# Read said CSVs using fread
read_data <- function(fn){
  dt_temp <- read.csv(fn, fileEncoding = "utf-8")
  return(dt_temp)
}

# Apply recursive reading function 
mylist <- lapply(all_files, read_data)
incubators_all_raw_content <- rbindlist( mylist , use.names = T)
incubators_all_raw_content <- incubators_all_raw_content[,-1]

# Write base file containing full content
write.csv(incubators_all_raw_content , 
          file = "data/incubators_raw_content.csv",
          fileEncoding = "utf-8",
          row.names = F)
