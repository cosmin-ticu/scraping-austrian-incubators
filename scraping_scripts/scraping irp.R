rm(list = ls())
library(tidyverse)
library(rvest)
library(xml2)
library(data.table)
library(magrittr)

# Get links of all Investment Ready Program articles

get_links_irp <- function(){
    
    irp_blog_page <- read_html("https://investment-ready.org/blog/")
    
    t_list <- list()
    
    t_list[['article_link']] <- irp_blog_page %>% html_nodes('.entry-snippet') %>%
      html_nodes('h2') %>% html_nodes('a') %>% html_attr('href')
    t_list[["img_link"]] <- "no_image" # no images to download, unfortunately
    print(paste("Currently scraping:","https://investment-ready.org/blog/"))
    return(data.frame(t_list))
}

irp_article_links <- get_links_irp()

# Write out the list of article and image links
write.csv(irp_article_links, file = 'irp_files/irp_article_links.csv')

get_articles_irp <- function(){
  ret_df <- rbindlist(lapply(irp_article_links[[1]], function(irp_article_url){
    t <- read_html(irp_article_url)
    print(paste("Currently scraping:",irp_article_url))
    t_list <- list()
    t_list[['creator']] <- 'InvestmentReadyProgram'
    t_list[['article_URL']] <- irp_article_url
    t_list[['article_title']] <- t %>% html_nodes('article') %>% 
      html_nodes('h1') %>%  html_text()
    t_list[['content']] <- t %>% html_nodes('.ninecol') %>% 
      html_nodes('article') %>% 
      html_nodes('.entry-content') %>% 
      html_text() %>% 
      trimws() %>% gsub('\\s+',' ',.)
    t_list[['date']] <- t %>% html_nodes('.ninecol') %>% html_nodes('article') %>% 
      html_nodes('.entry-meta') %>% html_nodes('.published') %>%  
      html_nodes('a') %>% html_attr('title')
    return(data.frame(t_list))
  }))
  return(ret_df)
}

irp_articles_content <- get_articles_irp()

irp_articles_content <- merge(irp_articles_content, 
                              irp_article_links, 
                              by.x = "article_URL", 
                              by.y = "article_link")

# Write out the list of articles & characteristics
write.csv(irp_articles_content, 
          file = 'irp_files/irp_articles_content.csv',
          fileEncoding = "utf-8")
