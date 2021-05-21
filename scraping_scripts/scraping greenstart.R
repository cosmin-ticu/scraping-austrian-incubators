rm(list = ls())
library(tidyverse)
library(rvest)
library(xml2)
library(data.table)
library(magrittr)

# Get links of all Greenstart articles (they have 6 pages of content)

get_links_greenstart <- function(){
  links_to_get <- paste0('https://greenstart.at/aktuelles//page/',
                         1:6)
  ret_df <- rbindlist(lapply(links_to_get, function(greenstart_url){
    
    greenstart_news_page <- read_html(greenstart_url)
    
    Sys.sleep(0.5)
    
    t <- greenstart_news_page %>% html_nodes('.postteaser__list')
    
    t_list <- list()
    
    t_list[['article_link']] <- t %>% 
      html_nodes('a') %>% html_attr('href')
    
    t_list[['img_link']] <- t %>% 
      html_nodes('.postteaser__list__item__image') %>%
      html_nodes('img') %>% html_attr('src')
    
    print(paste("Currently scraping:",greenstart_url))
    
    return(data.frame(t_list))
  }))
  return(ret_df)
}

greenstart_article_links <- get_links_greenstart()

# Write out the list of article and image links
write.csv(greenstart_article_links, file = 'greenstart_files/greenstart_article_links.csv')

get_articles_greenstart <- function(){
  ret_df <- rbindlist(lapply(greenstart_article_links[[1]], 
                             function(greenstart_article_url){
                               t <- read_html(greenstart_article_url)
                               
                               print(paste("Currently scraping:",
                                           greenstart_article_url))
                               
                               t_list <- list()
                               
                               t_list[['creator']] <- 'Greenstart'
                               
                               t_list[['article_URL']] <- greenstart_article_url
                               
                               t_list[['article_title']] <- t %>% 
                                 html_nodes('.singleheader__top__headline') %>% 
                                 html_text() %>% trimws()
                               
                               t_list[['content']] <- t %>% 
                                 html_nodes('.textblock') %>% 
                                 html_text() %>%
                                 paste(.,collapse = ' ') %>% 
                                 trimws() %>% 
                                 gsub('\\s+',' ', .)
                               
                               t_list[['date']] <- t %>% 
                                 html_nodes('.singleheader__top__subline') %>% 
                                 html_text() %>% 
                                 trimws()
                               
                               return(data.frame(t_list))
                             }))
  return(ret_df)
}

greenstart_articles_content <- get_articles_greenstart()

greenstart_articles_content <- merge(greenstart_articles_content, 
                                    greenstart_article_links, 
                                    by.x = "article_URL", 
                                    by.y = "article_link")

greenstart_articles_content <- greenstart_articles_content[!greenstart_articles_content$content == '']

# Write out the list of articles & characteristics
write.csv(greenstart_articles_content, 
          file = 'greenstart_files/greenstart_articles_content.csv', 
          fileEncoding = "utf-8")

