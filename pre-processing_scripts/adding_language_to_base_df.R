
# Housekeeping ------------------------------------------------------------

rm(list = ls())
library(aws.comprehend)
library(data.table)

# Data setup --------------------------------------------------------------

# Read base df of all content
all_incubators_raw_content <- read.csv(file = "data/incubators_raw_content.csv",
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

# Language detection with AWS ---------------------------------------------

# Function that detects main language of article through AWS comprehend according to first 400 characters
get_language_scores_400chars <- function(){
  ret_df <- rbindlist(lapply(all_incubators_raw_content[[4]], 
                             function(content){
                               
                               Sys.sleep(2)

                               t_list <- detect_language(substr(content, 1, 400))[1,2:3]
                               
                               return(data.frame(t_list))
                             }))
  return(ret_df)
}

language_scores <- get_language_scores_400chars()

all_incubators_raw_content_languages <- cbind(all_incubators_raw_content, language_scores)

# Write out the data ------------------------------------------------------

# Write base file containing full content
write.csv(all_incubators_raw_content_languages , 
          file = "data/incubators_raw_content_languages.csv",
          fileEncoding = "utf-8",
          row.names = F)

# Post analysis / extra AWS analysis --------------------------------------

# Language detection with AWS for article titles
# Function that detects main language of article through AWS comprehend according to first 400 characters
get_language_scores_titles <- function(){
  ret_df <- rbindlist(lapply(all_incubators_raw_content[[3]], 
                             function(content){
                               
                               Sys.sleep(2)
                               
                               t_list <- detect_language(content)[1,2:3]
                               
                               return(data.frame(t_list))
                             }))
  return(ret_df)
}

language_scores <- get_language_scores_titles()

all_incubators_raw_content_languages_titles <- cbind(all_incubators_raw_content, language_scores)

# Write out the data 
# Write base file containing full content
write.csv(all_incubators_raw_content_languages_titles , 
          file = "data/incubators_raw_content_languages_titles(ad-hoc).csv",
          fileEncoding = "utf-8",
          row.names = F)
