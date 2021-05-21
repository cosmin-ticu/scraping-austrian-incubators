rm(list = ls())
library(tidyverse)
library(rvest)
library(xml2)
library(data.table)
library(magrittr)

# Get links of all Match-maker articles (they have 5 pages of content - each 9 articles)

get_links_matchmaker <- function(){
  links_to_get <- paste0('https://www.match-maker.ventures/insights/page/',
                         1:5)
  ret_df <- rbindlist(lapply(links_to_get, function(matchmaker_url){
    
    matchmaker_insights_page <- read_html(matchmaker_url)
    
    t <- matchmaker_insights_page %>% html_nodes('.insights-list')
    
    t_list <- list()
    
    t_list[['article_link']] <- t %>% 
      html_nodes('.insights-item') %>% 
      html_attr('href')
    
    t_list[['img_link']] <- t %>% 
      html_nodes('a') %>% 
      html_nodes('img') %>% 
      html_attr('src')
    
    print(paste("Currently scraping:",matchmaker_url))
    
    return(data.frame(t_list))
  }))
  return(ret_df)
}

matchmaker_article_links <- get_links_matchmaker()

# Write out the list of article and image links
write.csv(matchmaker_article_links, file = 'matchmaker_files/matchmaker_article_links.csv')

# Scrape the contents of each article

get_articles_matchmaker <- function(){
  ret_df <- rbindlist(lapply(matchmaker_article_links[[1]], function(matchmaker_article_url){
    t <- read_html(matchmaker_article_url)
    
    print(paste("Currently scraping:",matchmaker_article_url))
    
    t_list <- list()
    
    t_list[['creator']] <- 'MatchMakerVentures'
    
    t_list[['article_URL']] <- matchmaker_article_url
    
    t_list[['article_title']] <- t %>% html_nodes('h1') %>% 
      html_text() %>% extract2(1) %>% trimws()
    
    t_list[['content']] <- t %>% html_nodes('.article-container') %>% 
      html_text() %>% 
      gsub('\\s+',' ',.) %>% 
      trimws()
    
    t_list[['date']] <- t %>% 
      html_nodes('.text-wrapper') %>% 
      html_nodes('h5') %>% 
      html_nodes('span') %>% html_text()
    
    return(data.frame(t_list))
  }))
  return(ret_df)
}

matchmaker_articles_content <- get_articles_matchmaker()

matchmaker_articles_content <- merge(matchmaker_articles_content, 
                                     matchmaker_article_links, 
                                     by.x = "article_URL", 
                                     by.y = "article_link")

# Write out the list of articles & characteristics
write.csv(matchmaker_articles_content, 
          file = 'matchmaker_files/matchmaker_articles_content.csv',
          fileEncoding = "utf-8")
