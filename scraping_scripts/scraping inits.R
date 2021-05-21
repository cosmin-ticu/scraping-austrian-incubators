rm(list = ls())
library(tidyverse)
library(rvest)
library(xml2)
library(data.table)
library(magrittr)

# Get links of all INiTS articles in German (they have 5 pages of content)

get_links_inits_DE <- function(){
  
  links_to_get <- paste0('https://www.inits.at/news/',
                         1:5)
  
  ret_df <- rbindlist(lapply(links_to_get, function(inits_url){
    
    inits_news_page <- read_html(inits_url)
    
    t <- inits_news_page %>% html_nodes('.elementor-widget-container')
    
    t_list <- list()
    
    t_list[['article_link']] <- t %>% 
      html_nodes('.elementor-post__card') %>%
      html_nodes('.elementor-post__thumbnail__link') %>% 
      html_attr('href')
    
    t_list[['img_link']] <- t %>% 
      html_nodes('.elementor-post__card') %>%
      html_nodes('.elementor-post__thumbnail') %>% 
      html_nodes('img') %>% html_attr('src')
    
    t_list[['date']] <- t %>% 
      html_nodes(".elementor-post-date") %>% 
      html_text() %>% 
      trimws()
    
    print(paste("Currently scraping:",inits_url))
    
    return(data.frame(t_list))
  }))
  return(ret_df)
}

inits_article_links_DE <- get_links_inits_DE()

# 3 articles showed up twice on account of having their own banner above the list
inits_article_links_DE <- inits_article_links_DE[!duplicated(inits_article_links_DE$article_link)]

# Get links of all INiTS articles in English (they have 2 pages of content)

get_links_inits_EN <- function(){
  
  links_to_get <- paste0('https://www.inits.at/en/news/',
                         1:2)
  
  ret_df <- rbindlist(lapply(links_to_get, function(inits_url){
    
    inits_news_page <- read_html(inits_url)
    
    t <- inits_news_page %>% html_nodes('.elementor-post')
    
    t_list <- list()
    
    t_list[['article_link']] <- t %>% 
      html_nodes('.elementor-post__read-more') %>%
      html_attr('href')
    
    t_list[['img_link']] <- t %>% 
      html_nodes('.elementor-post__thumbnail') %>% 
      html_nodes('img') %>% html_attr('src')
    
    t_list[['date']] <- t %>% 
      html_nodes(".elementor-post-date") %>% 
      html_text() %>% 
      trimws()
    
    print(paste("Currently scraping:",inits_url))
    
    return(data.frame(t_list))
  }))
  return(ret_df)
}

inits_article_links_EN <- get_links_inits_EN()

# Concatenate the 2 datasets

inits_article_links <- rbind.data.frame(inits_article_links_DE, 
                                        inits_article_links_EN)

# Write out the list of article and image links
write.csv(inits_article_links, file = 'inits_files/inits_article_links.csv')

inits_article_url <- inits_article_links[[1]][63]

get_articles_inits <- function(){
  ret_df <- rbindlist(lapply(inits_article_links[[1]], 
                             function(inits_article_url){
                               t <- read_html(inits_article_url)
                               
                               print(paste("Currently scraping:",inits_article_url))
                               
                               t_list <- list()
                               
                               t_list[['creator']] <- 'INiTS'
                               
                               t_list[['article_URL']] <- inits_article_url
                               
                               t_list[['article_title']] <- t %>% 
                                 html_nodes("main") %>% 
                                 html_node(".elementor-heading-title.elementor-size-default") %>% 
                                 html_text()
                               
                               t_list[['content']] <- t %>% 
                                 html_nodes("main") %>% 
                                 html_nodes(".elementor-element > .elementor-element-populated") %>% 
                                 html_nodes(".elementor-widget-wrap")%>% 
                                 html_text()  %>% 
                                 extract2(2) %>% 
                                 trimws() %>% 
                                 gsub('\\s+',' ',.)
                               
                               return(data.frame(t_list))
                             }))
  return(ret_df)
}

inits_articles_content <- get_articles_inits()

inits_articles_content <- merge(inits_articles_content, 
                                         inits_article_links,
                                         by.x = "article_URL",
                                         by.y = "article_link")

# Write out the list of articles & characteristics
write.csv(inits_articles_content, 
          file = 'inits_files/inits_articles_content.csv', 
          fileEncoding = "utf-8")

