library(data.table)

## First put all file names into a list 
all_files <- list.files(pattern = "content.csv", recursive = T)

## Read data using fread
read_data <- function(fn){
  dt_temp <- read.csv(fn, fileEncoding = "utf-8")
  return(dt_temp)
  
}
# then using 
mylist <- lapply(all_files, read_data)
incubators_all_raw_content <- rbindlist( mylist , use.names = T)
incubators_all_raw_content <- incubators_all_raw_content[,-1]

# Write base file containing full content
write.csv(incubators_all_raw_content , 
          file = "data/incubators_all_raw_content.csv",
          fileEncoding = "utf-8")
