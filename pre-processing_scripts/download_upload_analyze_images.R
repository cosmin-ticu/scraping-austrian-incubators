
# Housekeeping ------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(httr)
library(aws.s3)
library(data.table)
library(paws.machine.learning)

# Data setup --------------------------------------------------------------

# Read base df of all content
incubators_raw_content <- read.csv(file = "data/incubators_raw_content.csv",
                                       fileEncoding = "utf-8")

# Set up AWS in R
keyTable <- read.csv("D:/OneDrive - Central European University/Courses/Spring_Term/Data Science 3/accessKeys.csv", 
                     header = T) # accessKeys.csv == the CSV downloaded from AWS containing your Access & Secret keys
AWS_ACCESS_KEY_ID <- as.character(keyTable$Access.key.ID)
AWS_SECRET_ACCESS_KEY <- as.character(keyTable$Secret.access.key)

#activate
Sys.setenv("AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
           "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
           "AWS_DEFAULT_REGION" = "eu-west-1") 

# Download all images -----------------------------------------------------

# Keep only links
imgs_to_download <- incubators_raw_content[
  incubators_raw_content$img_link != "no_image",] %>% 
  select(c(img_link,creator))

# Download all of the images from every incubator 
# - unfortunately, URLs that contain special characters are excluded
for (i in 1:nrow(imgs_to_download)) {
  
  if(!is.na(imgs_to_download$img_link[i]) & 
     !http_error(imgs_to_download$img_link[i])){
    
    tryCatch(download.file(imgs_to_download$img_link[i],
                  destfile = paste0("incubators_images/", 
                                    imgs_to_download$creator[i],
                                    i,'.jpg'),
                  mode = 'wb'),
             
             error = function(e) print(paste0(imgs_to_download$img_link[i], 
                                              " did not download")))
    
  }
  Sys.sleep(0.5)
}

# AWS S3 ------------------------------------------------------------------

# Upload all images to S3 for processing with AWS Rekognition
s3sync(path = 'incubators_images', 
       bucket = 'cosmin-ceu-2020', 
       direction = 'upload', 
       verbose = T, 
       recursive = T)

# AWS Rekognition ---------------------------------------------------------

# Define list of incubators to search image for
incubator_names <- setdiff(unique(incubators_raw_content$creator),
                           c("TechHouse","InvestmentReadyProgram"))

# Detect the entities in every image
all_incubator_labels <- rbindlist(
  lapply(incubator_names,function(incubator) {
  
    # Initialize AWS
    svc <- paws.machine.learning::rekognition(
      config = list(region = "eu-west-1"))
    
    # Define image names to search for
    picture <- get_bucket_df(prefix = incubator,bucket = 'cosmin-ceu-2020') %>% 
      select("Key") %>% 
      list()
    picture <- picture[[1]]$Key
  
    ret_df <- rbindlist(lapply(picture, function(x){
      
      # Entity recognition through AWS
      gicu <- svc$detect_labels(list(
        S3Object = list(
          Bucket = "cosmin-ceu-2020",
          Name = x
        )), MaxLabels = 10)
      
      # Convert output into a friendly data frame
      df <- lapply(gicu$Labels, `[`, c('Name', 'Confidence'))
      df <- rbindlist(df)
      return(df)
      
    }))
    
    # Combine all picture outputs into data frame with incubator label
    out_df <- ret_df %>% 
      group_by(Name) %>% 
      summarise(Label_Confidence = mean(Confidence),
                count = n())
    out_df["Creator"] <- incubator
    return(out_df)
  
}))

# Write out the data ------------------------------------------------------

# Write image label recognition file for analysis
write.csv(all_incubator_labels , 
          file = "data/incubators_image_10_entities.csv",
          fileEncoding = "utf-8",
          row.names = F)
