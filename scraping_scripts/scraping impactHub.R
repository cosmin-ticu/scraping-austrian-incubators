rm(list = ls())
library(tidyverse)
library(rvest)
library(xml2)
library(data.table)
library(magrittr)

# Get links of all Impact Hub articles (they have 20 pages of content back to 2015)

get_links_impactHub <- function(){
  links_to_get <- paste0('https://vienna.impacthub.net/blog/page/',
                         1:20)
  ret_df <- rbindlist(lapply(links_to_get, function(impactHub_url){
    
    impactHub_news_page <- read_html(impactHub_url)
    
    Sys.sleep(0.5)
    
    t <- impactHub_news_page %>% html_nodes('.c-card')
    
    t_list <- list()
    
    t_list[['article_link']] <- t %>% 
      html_nodes('.c-card__content') %>%
      html_nodes('a') %>% html_attr('href')
    
    t_list[['img_link']] <- ifelse(grepl("<img", t %>% 
                                           html_nodes('.c-media') %>%
                                           html_nodes('a')), t %>% 
                                     html_nodes('.c-media') %>%
                                     html_nodes('img') %>% html_attr('src'),
                                   "no_image")
    
    print(paste("Currently scraping:",impactHub_url))
    
    return(data.frame(t_list))
  }))
  return(ret_df)
}

impactHub_article_links <- get_links_impactHub()

# The articles without images are written in a different format --> remove them (small group anyway)
impactHub_article_links <- impactHub_article_links[impactHub_article_links$img_link != "no_image",]

# Download images outside of the scraping loop
count <- 1
for (i in impactHub_article_links$img_link) {
  if(!is.na(i) & !http_error(i)){
    download.file(i,
                  destfile = paste0("impactHub_files/impactHub_img",
                                    count,'.png'),
                  mode = 'wb')
  }
  count <- count + 1
}

# Write out the list of article and image links
write.csv(impactHub_article_links, file = 'impactHub_files/impactHub_article_links.csv')

impactHub_article_url <- impactHub_article_links[[1]][10]

get_articles_impactHub <- function(){
  ret_df <- rbindlist(lapply(impactHub_article_links[[1]], 
                             function(impactHub_article_url){
                               t <- read_html(impactHub_article_url)
                               
                               print(paste("Currently scraping:",impactHub_article_url))
                               
                               t_list <- list()
                               
                               t_list[['creator']] <- 'ImpactHub'
                               
                               t_list[['article_URL']] <- impactHub_article_url
                               
                               t_list[['article_title']] <- t %>% 
                                 html_nodes('.c-banner') %>% 
                                 html_nodes(".c-heading") %>% 
                                 html_text() %>% trimws()
                               
                               t_list[['content']] <- t %>% html_nodes('.row') %>% 
                                 html_nodes('.l-spacer') %>% 
                                 html_text() %>% 
                                 extract2(1) %>% 
                                 trimws() %>% 
                                 gsub('\\s+',' ', .)
                               
                               t_list[['date']] <- t %>% html_nodes('.c-banner') %>% 
                                 html_nodes(".c-byline") %>% 
                                 html_text() %>% strsplit(., "\\|") %>% 
                                 extract2(1) %>% extract2(1) %>% 
                                 trimws()
                               
                               return(data.frame(t_list))
                             }))
  return(ret_df)
}

impactHub_articles_content <- get_articles_impactHub()

impactHub_articles_content <- merge(impactHub_articles_content, 
                                    impactHub_article_links, 
                                    by.x = "article_URL", 
                                    by.y = "article_link")

# Write out the list of articles & characteristics
write.csv(impactHub_articles_content, 
          file = 'impactHub_files/impactHub_articles_content.csv',
          fileEncoding = "utf-8")

