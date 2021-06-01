rm(list = ls())
library(tidyverse)
library(rvest)
library(xml2)
library(data.table)
library(magrittr)
library(lubridate)

# Get links of all A1 Startup articles (they have 7 pages of content)

get_links_a1 <- function(){

  count <- 1
  counter <- 1
  
  links_to_get <- paste0('https://www.a1startup.net/news/page/',
                         1:7)
  
  ret_df <- rbindlist(lapply(links_to_get, function(a1_url){

    a1_news_page <- read_html(a1_url)
    
    Sys.sleep(0.5)
    
    t <- a1_news_page %>% html_nodes('.news-article')
    
    t_list <- list()
    
    t_list[['article_link']] <- t %>% 
      html_nodes('.entry-header') %>% 
      html_nodes('a') %>% html_attr('href')
    
    t_list[['img_link']] <- t %>% 
      html_nodes('.post-image') %>%
      html_nodes('img') %>% html_attr('src')
    
    for (i in t_list[['img_link']]) {
      if(!is.na(t_list[['img_link']][count])){
        download.file(t_list[['img_link']][count], 
                      destfile = paste0("a1_files/a1_img", counter,'.png'), 
                      mode = 'wb')
      }
      count <- count + 1
      counter <<- counter + 1
    }
    
    print(paste("Currently scraping:",a1_url))
    
    return(data.frame(t_list))
  }))
  
  return(ret_df)
}

a1_article_links <- get_links_a1()

values <- seq(from = as.Date("2015-01-01"), to = as.Date("2021-01-01"), 
              by = 'month')

values <- rev(tail(values, nrow(a1_article_links)))

a1_article_links$date <- values

# Write out the list of article and image links
write.csv(a1_article_links, file = 'a1_files/a1_article_links.csv')

get_articles_a1 <- function(){
  ret_df <- rbindlist(lapply(a1_article_links[[1]], 
                             function(a1_article_url){
                               t <- read_html(a1_article_url)
                               
                               print(paste("Currently scraping:",
                                           a1_article_url))
                               
                               t_list <- list()
                               
                               t_list[['creator']] <- 'A1Startup'
                               
                               t_list[['article_URL']] <- a1_article_url
                               
                               t_list[['article_title']] <- t %>% 
                                 html_nodes('.entry-title') %>% 
                                 html_text() %>% trimws()
                               
                               t_list[['content']] <- t %>% 
                                 html_nodes('.entry-content') %>% 
                                 html_text() %>%
                                 trimws() %>% 
                                 gsub('\\s+',' ', .)
                               
                               return(data.frame(t_list))
                             }))
  return(ret_df)
}

a1_articles_content <- get_articles_a1()

a1_articles_content <- merge(a1_articles_content, 
                                     a1_article_links, 
                                     by.x = "article_URL", 
                                     by.y = "article_link")

# Write out the list of articles & characteristics
write.csv(a1_articles_content, 
          file = 'a1_files/a1_articles_content.csv', 
          fileEncoding = "utf-8")

