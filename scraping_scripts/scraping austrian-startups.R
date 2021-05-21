rm(list = ls())
library(tidyverse)
library(rvest)
library(xml2)
library(data.table)
library(magrittr)

# Get links of all AustrianStartups articles

get_links_austrianStartups <- function(){
  
  austrianStartups_news_page <- read_html("https://austrianstartups.com/startup-news/")
  
  t_list <- list()
  
  t_list[['article_link']] <- austrianStartups_news_page %>% 
    html_nodes('.posts-widget-sng') %>% 
    html_nodes('.posts-widget-content') %>% 
    html_node('a') %>% html_attr('href')
  
  t_list[['img_link']] <- austrianStartups_news_page %>% 
    html_nodes('.posts-widget-sng') %>% 
    html_nodes('.posts-widget-img') %>% 
    html_nodes('noscript') %>% 
    html_nodes('img') %>% html_attr('src')
  
  print(paste("Currently scraping:","https://austrianstartups.com/startup-news/"))
  
  return(data.frame(t_list))
}

austrianStartups_article_links <- get_links_austrianStartups()

# Write out the list of article and image links
write.csv(austrianStartups_article_links, file = 'austrianStartups_files/austrianStartups_article_links.csv')

# Scrape the contents of each article

get_articles_austrianStartups <- function(){
  ret_df <- rbindlist(lapply(austrianStartups_article_links[[1]], 
                             function(austrianStartups_article_url){
                               
    t <- read_html(austrianStartups_article_url)
    
    print(paste("Currently scraping:",austrianStartups_article_url))
    
    t_list <- list()
    
    t_list[['creator']] <- 'AustrianStartups'
    
    t_list[['article_URL']] <- austrianStartups_article_url
    
    t_list[['article_title']] <- t %>% 
      html_nodes('.header-title') %>% 
      html_nodes('h1') %>% 
      html_text()
    
    t_list[['content']] <- t %>% 
      html_nodes('.section-wrapper') %>% 
      html_nodes('.post-section') %>% 
      html_text() %>% trimws() %>% 
      gsub('\\s+',' ', .) %>% 
      gsub('\\[…]','',.)
    
    t_list[['date']] <- t %>% html_nodes('.section-content') %>% 
      html_nodes('.header-post-meta') %>% 
      html_text() %>% 
      trimws()
    
    return(data.frame(t_list))
    
  }))
  return(ret_df)
}

austrianStartups_articles_content <- get_articles_austrianStartups()

austrianStartups_articles_content <- austrianStartups_articles_content[austrianStartups_articles_content$content != ""]

austrianStartups_articles_content <- merge(austrianStartups_articles_content, 
                                         austrianStartups_article_links,
                                         by.x = "article_URL",
                                         by.y = "article_link")

# Write out the list of articles & characteristics
write.csv(austrianStartups_articles_content, 
          file = 'austrianStartups_files/austrianStartups_articles_content.csv',
          fileEncoding = "utf-8")
