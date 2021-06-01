rm(list = ls())
library(tidyverse)
library(rvest)
library(xml2)
library(data.table)
library(magrittr)

# Get links of all Techhouse articles

get_links_techhouse <- function(){
  
  techhouse_news_page <- read_html("https://www.tech-house.io/news-and-events/")
  
  t_list <- list()
  
  t_list[['article_link']] <- techhouse_news_page %>% 
    html_nodes('.wpb_wrapper') %>% html_nodes('h4') %>% 
    html_node('a') %>% html_attr('href')
  
  t_list[['img_link']] <- techhouse_news_page %>% html_nodes('.boxy') %>% 
    html_node('.wpb_single_image') %>% html_node('figure') %>% 
    html_node('a') %>% html_node('img') %>% html_attr('src')
  print(paste("Currently scraping:","https://www.tech-house.io/news-and-events/"))
  return(data.frame(t_list))
}

techhouse_article_links <- get_links_techhouse()

techhouse_article_links[[1]][1] <- paste0("https://www.tech-house.io",techhouse_article_links[[1]][1])
techhouse_article_links[[1]][2] <- paste0("https://www.tech-house.io",techhouse_article_links[[1]][2])
techhouse_article_links[[1]][4] <- paste0("https://www.tech-house.io",techhouse_article_links[[1]][4])

# Download images outside of the scraping loop
count <- 1
for (i in techhouse_article_links$img_link) {
  if(!is.na(i) & !http_error(i)){
    download.file(i,
                  destfile = paste0("techhouse_files/techhouse_img",
                                    count,'.png'),
                  mode = 'wb')
  }
  count <- count + 1
}

# Write out the list of article and image links
write.csv(techhouse_article_links, file = 'techhouse_files/techhouse_article_links.csv')


get_articles_techhouse <- function(){
  ret_df <- rbindlist(lapply(techhouse_article_links[[1]], 
                             function(techhouse_article_url){
                               t <- read_html(techhouse_article_url)
                               print(paste("Currently scraping:",techhouse_article_url))
                               t_list <- list()
                               t_list[['creator']] <- 'TechHouse'
                               t_list[['article_URL']] <- techhouse_article_url
                               t_list[['article_title']] <- t %>% html_nodes(".crane-page-title-heading") %>% html_text()
                               t_list[['content']] <- t %>% 
                                 html_node('.blog-single-post__txt-wrapper') %>% 
                                 html_text() %>% 
                                 trimws() %>% gsub('\\n+',' ',.)
                               t_list[['date']] <- t %>% 
                                 html_node('.crane-post-meta') %>% 
                                 html_text() %>% 
                                 trimws() %>% gsub('Posted on ','',.)
                               return(data.frame(t_list))
                             }))
  return(ret_df)
}

techhouse_articles_content <- get_articles_techhouse()

techhouse_articles_content <- merge(techhouse_articles_content, techhouse_article_links, by.x = "article_URL", by.y = "article_link")

# Write out the list of articles & characteristics
write.csv(techhouse_articles_content, 
          file = 'techhouse_files/techhouse_articles_content.csv',
          fileEncoding = "utf-8")
