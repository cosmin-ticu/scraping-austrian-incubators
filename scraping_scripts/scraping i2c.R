rm(list = ls())
library(tidyverse)
library(rvest)
library(xml2)
library(data.table)
library(magrittr)
library(stringr)

# Get links of all I2C articles

get_links_i2c <- function(){
  links_to_get <- paste0('https://i2c.tuwien.ac.at/event/page/',
                         1:5)
  ret_df <- rbindlist(lapply(links_to_get, function(i2c_url){
    i2c_url <- 'https://i2c.tuwien.ac.at/event/page/2'
    i2c_news_page <- read_html(i2c_url)
    
    t <- i2c_news_page %>%
      html_nodes('.post-content')
    
    t_list <- list()
    
    t_list[['article_link']] <- t %>% html_nodes('.post-title') %>% 
      html_nodes('a') %>% 
      html_attr('href')
    
    t_list[['img_link']] <- ifelse(grepl("<img", 
                                         t %>% html_nodes('.post-thumbnail')), 
                                   t %>% html_nodes('.post-thumbnail') %>% 
                                     html_nodes('img') %>% 
                                     html_attr('src'),
                                   "no_image")
    
    for (i in t_list[['img_link']]) {
      if(!is.na(t_list[['img_link']][count]) & !http_error(t_list[['img_link']][count])){
        download.file(t_list[['img_link']][count], 
                      destfile = paste0("i2c_files/i2c_img", 
                                        counter,'.png'), 
                      mode = 'wb')
      }
      count <- count + 1
      counter <<- counter + 1
    }
    
    print(paste("Currently scraping:",i2c_url))
    
    return(data.frame(t_list))
  }))
  return(ret_df)
}

i2c_article_links <- get_links_i2c()

# Download images outside of the scraping loop
count <- 1
for (i in i2c_article_links$img_link[i2c_article_links$img_link != "no_image"]) {
  if(!is.na(i) & !http_error(i)){
    download.file(i,
                  destfile = paste0("i2c_files/i2c_img",
                                    count,'.png'),
                  mode = 'wb')
  }
  count <- count + 1
}

# Write out the list of article and image links
write.csv(i2c_article_links, file = 'i2c_files/i2c_article_links.csv')


# Scrape the contents of each article

get_articles_i2c <- function(){
  ret_df <- rbindlist(lapply(i2c_article_links[[1]], 
                             function(i2c_article_url){
                               
    t <- read_html(i2c_article_url)
    
    print(paste("Currently scraping:",i2c_article_url))
    t_list <- list()
    
    t_list[['creator']] <- 'I2C'
    
    t_list[['article_URL']] <- i2c_article_url
    
    t_list[['article_title']] <- t %>% html_nodes('.hero-title') %>% 
      html_text() %>% trimws()
    
    t_list[['content']] <- t %>% html_nodes('.post-content-inner') %>% 
      html_nodes('p') %>% 
      html_text() %>% 
      trimws() %>% 
      gsub('\\s+',' ', .) %>% 
      paste(.,collapse = ' ')
    
    t_list[['date']] <- t %>% html_nodes('.post-date') %>% html_text()
    return(data.frame(t_list))
  }))
  return(ret_df)
}

i2c_articles_content <- get_articles_i2c()

i2c_articles_content <- i2c_articles_content[i2c_articles_content$content != ""]

i2c_articles_content <- merge(i2c_articles_content, 
                              i2c_article_links, 
                              by.x = "article_URL", 
                              by.y = "article_link")

# Write out the list of articles & characteristics
write.csv(i2c_articles_content, 
          file = 'i2c_files/i2c_articles_content.csv',
          fileEncoding = "utf-8")
