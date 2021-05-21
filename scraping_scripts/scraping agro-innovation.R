rm(list = ls())
library(tidyverse)
library(rvest)
library(xml2)
library(data.table)
library(magrittr)

# Get links of all Agro Innovation Lab articles in German (they have 8 pages of content)

get_links_agroInnovation_DE <- function(){
  
  links_to_get <- paste0('https://www.agroinnovationlab.com/de/news/page/',
                         1:8)
  
  ret_df <- rbindlist(lapply(links_to_get, function(agroInnovation_url){
    
    agroInnovation_news_page <- read_html(agroInnovation_url)
    
    t <- agroInnovation_news_page %>% html_nodes('.insights-list')
    
    t_list <- list()
    
    t_list[['article_link']] <- agroInnovation_news_page %>% 
      html_nodes('.fusion-builder-row.fusion-row') %>%
      html_nodes('.fusion-post-wrapper') %>% html_nodes('h2') %>% 
      html_nodes('a') %>% html_attr('href')
    
    t_list[['img_link']] <- agroInnovation_news_page %>% 
      html_nodes('.fusion-builder-row.fusion-row') %>%
      html_nodes('.fusion-post-wrapper') %>% 
      html_nodes('.fusion-image-wrapper') %>% 
      html_nodes('img') %>% html_attr('src')
    
    print(paste("Currently scraping:",agroInnovation_url))
    
    return(data.frame(t_list))
  }))
  return(ret_df)
}

agroInnovation_article_links_DE <- get_links_agroInnovation_DE()

# Get links of all Agro Innovation Lab articles in English (they have 6 pages of content)

get_links_agroInnovation_EN <- function(){
  
  links_to_get <- paste0('https://www.agroinnovationlab.com/news/page/',
                         1:6)
  
  ret_df <- rbindlist(lapply(links_to_get, function(agroInnovation_url){
    
    agroInnovation_news_page <- read_html(agroInnovation_url)
    
    t <- agroInnovation_news_page %>% html_nodes('.insights-list')
    
    t_list <- list()
    
    t_list[['article_link']] <- agroInnovation_news_page %>% 
      html_nodes('.fusion-builder-row.fusion-row') %>%
      html_nodes('.fusion-post-wrapper') %>% html_nodes('h2') %>% 
      html_nodes('a') %>% html_attr('href')
    
    t_list[['img_link']] <- agroInnovation_news_page %>% 
      html_nodes('.fusion-builder-row.fusion-row') %>%
      html_nodes('.fusion-post-wrapper') %>% 
      html_nodes('.fusion-image-wrapper') %>% 
      html_nodes('img') %>% html_attr('src')
    
    print(paste("Currently scraping:",agroInnovation_url))
    
    return(data.frame(t_list))
  }))
  return(ret_df)
}

agroInnovation_article_links_EN <- get_links_agroInnovation_EN()

# Concatenate the 2 datasets

agroInnovation_article_links <- rbind.data.frame(agroInnovation_article_links_DE, 
                                                 agroInnovation_article_links_EN)

# Write out the list of article and image links
write.csv(agroInnovation_article_links, file = 'agroInnovation_files/agroInnovation_article_links.csv')

get_articles_agroInnovation <- function(){
  ret_df <- rbindlist(lapply(agroInnovation_article_links[[1]], 
                             function(agroInnovation_article_url){
                               t <- read_html(agroInnovation_article_url)
                               
                               print(paste("Currently scraping:",agroInnovation_article_url))
                               
                               t_list <- list()
                               
                               t_list[['creator']] <- 'AgroInnovationLab'
                               
                               t_list[['article_URL']] <- agroInnovation_article_url
                               
                               t_list[['article_title']] <- t %>% html_nodes(".fusion-post-title-meta-wrap") %>% 
                                 html_nodes('.entry-title') %>% html_text()
                               
                               t_list[['content']] <- t %>% 
                                 html_nodes(".post-content")%>% 
                                 html_text() %>% 
                                 trimws() %>% 
                                 gsub('\\s+',' ',.)
                               
                               t_list[['date']] <- t %>% html_nodes(".fusion-meta-info-wrapper") %>% 
                                 html_nodes('span') %>% html_text() %>% extract2(4)
                               
                               return(data.frame(t_list))
                             }))
  return(ret_df)
}

agroInnovation_articles_content <- get_articles_agroInnovation()

agroInnovation_articles_content <- merge(agroInnovation_articles_content, 
                                         agroInnovation_article_links,
                                         by.x = "article_URL",
                                         by.y = "article_link")

# Write out the list of articles & characteristics
write.csv(agroInnovation_articles_content, 
          file = 'agroInnovation_files/agroInnovation_articles_content.csv',
          fileEncoding = "utf-8")

