rm(list = ls())
library(tidyverse)
library(rvest)
library(xml2)
library(data.table)
library(magrittr)
library(stringr)

# Get links of all Factory1 articles

get_links_factory1 <- function(){
    factory1_url <- "https://www.trendingtopics.at/?s=factory1"
    
    factory1_news_page <- read_html(factory1_url)
    
    t <- factory1_news_page %>%
      html_nodes('.archive-list') %>%
      html_nodes('article')
    
    url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
    
    t_list <- list()
    
    t_list[['article_link']] <- t %>% html_nodes('.entry-title') %>% 
      html_nodes('a') %>% 
      html_attr('href')
    
    t_list[['img_link']] <- t %>% html_nodes('.entry-image-container') %>% 
      html_nodes('a') %>% 
      html_attr('style') %>% 
      str_extract(url_pattern) %>% 
      gsub(')','',.)
    
    t_list[['sponsored']] <- t %>% html_nodes('.entry-kicker') %>% 
      html_text() %>% 
      trimws()
    
    print(paste("Currently scraping:","https://www.trendingtopics.at/?s=factory1"))
    
    return(data.frame(t_list))
}

factory1_article_links <- get_links_factory1()

factory1_article_links <- factory1_article_links[factory1_article_links$sponsored == "Werbung",]

factory1_article_links <- subset(factory1_article_links, select = -sponsored)

factory1_article_links <- factory1_article_links %>% add_row(article_link = "https://www.trendingtopics.at/the-big-idea-of-maas-you-dont-need-a-car-anymore/",
                                   img_link = "https://www.trendingtopics.at/wp-content/uploads/2019/02/MaaS_Pexels-780x439.jpg")

factory1_article_links <- factory1_article_links %>% add_row(article_link = "https://www.trendingtopics.at/these-5-international-startups-made-it-to-the-factory1-accelerator-of-kapsch/",
                                                             img_link = "https://www.trendingtopics.at/wp-content/uploads/2019/02/factory1-644x362.jpg")

# Download images outside of loop
count <- 1

for (i in factory1_article_links$img_link) {
  if(!http_error(factory1_article_links$img_link[count])){
    download.file(factory1_article_links$img_link[count], 
                  destfile = paste0("factory1_files/factory1_img", 
                                    count,'.png'), 
                  mode = 'wb')
  }
  count <- count + 1
}

# Write out the list of article and image links
write.csv(factory1_article_links, file = 'factory1_files/factory1_article_links.csv')

# Scrape the contents of each article

get_articles_factory1 <- function(){
  ret_df <- rbindlist(lapply(factory1_article_links[[1]], 
                             function(factory1_article_url){
                               
    t <- read_html(factory1_article_url)
    
    print(paste("Currently scraping:",factory1_article_url))
    
    t_list <- list()
    
    t_list[['creator']] <- 'Factory1'
    
    t_list[['article_URL']] <- factory1_article_url
    
    t_list[['article_title']] <- t %>% html_nodes('.entry-header') %>% 
      html_nodes('.entry-title') %>% 
      html_text() %>% 
      extract2(1)
    
    t_list[['content']] <- t %>% 
      html_nodes('.entry-text') %>% 
      html_text() %>% 
      gsub('\\s+',' ', .) %>% 
      trimws()
    
    t_list[['date']] <- t %>% html_nodes('.entry-header') %>% 
      html_nodes('.entry-date.published') %>% 
      html_text() %>% sub("\\,.*", "", .)
    
    return(data.frame(t_list))
  }))
  return(ret_df)
}

factory1_articles_content <- get_articles_factory1()

factory1_articles_content <- merge(factory1_articles_content, 
                                   factory1_article_links, 
                                   by.x = "article_URL", 
                                   by.y = "article_link")

# Write out the list of articles & characteristics
write.csv(factory1_articles_content, 
          file = 'factory1_files/factory1_articles_content.csv',
          fileEncoding = "utf-8")
