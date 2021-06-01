rm(list = ls())
library(tidyverse)
library(rvest)
library(xml2)
library(data.table)
library(magrittr)
library(httr)

# Get links of all Impact Hub articles (they have 20 pages of content back to 2015)

get_links_blueminds <- function(){
  
    blueminds_news_page <- read_html("D:/OneDrive - Central European University/Courses/Spring_Term/Data Science 3/Term Project/blueminds_files/webpage.html")
    
    Sys.sleep(0.5)
    
    t <- blueminds_news_page %>% html_nodes('.blog-item-wrap')
    
    t_list <- list()
    
    t_list[['article_link']] <- ifelse(grepl("img-wrap", t), t %>% 
                                         html_nodes('.link-to-post') %>% html_attr('href'),
                                       "no_link")
      
    t_list[['img_link']] <- ifelse(grepl("img-wrap", t), t %>% 
                                     html_nodes('.img-wrap') %>%
                                     html_nodes('img') %>% html_attr('src'),
                                   "no_image")
    
    print(paste("Currently scraping:","https://www.blueminds-company.com/media/"))
    
    return(data.frame(t_list))
}

blueminds_article_links <- get_links_blueminds()

# The articles without images are written in a different format --> remove them (small group anyway)
blueminds_article_links <- blueminds_article_links[blueminds_article_links$img_link != "no_image",]

# Download images outside of the scraping loop
count <- 1
for (i in blueminds_article_links$img_link) {
  if(!is.na(blueminds_article_links$img_link[count]) & !http_error(blueminds_article_links$img_link[count])){
    download.file(blueminds_article_links$img_link[count],
                  destfile = paste0("blueminds_files/blueminds_img",
                                    count,'.png'),
                  mode = 'wb')
  }
  count <- count + 1
}

# Some articles showed up twice on account of having external links as well
blueminds_article_links <- blueminds_article_links[!duplicated(blueminds_article_links$article_link),]

values <- seq(from = as.Date("2015-01-01"), to = as.Date("2021-01-01"), by = '4 months')

values <- rev(tail(values, nrow(blueminds_article_links)))

blueminds_article_links$date <- values

# Write out the list of article and image links
write.csv(blueminds_article_links, file = 'blueminds_files/blueminds_article_links.csv')

blueminds_article_url <- blueminds_article_links[[1]][1]

get_articles_blueminds <- function(){
  ret_df <- rbindlist(lapply(blueminds_article_links[[1]], 
                             function(blueminds_article_url){
                               t <- read_html(blueminds_article_url)
                               
                               print(paste("Currently scraping:",blueminds_article_url))
                               
                               t_list <- list()
                               
                               t_list[['creator']] <- 'BlueMinds-Company'
                               
                               t_list[['article_URL']] <- blueminds_article_url
                               
                               t_list[['article_title']] <- t %>% 
                                 html_nodes('.heading-text') %>% 
                                 html_text() %>% trimws()
                               
                               t_list[['content']] <- t %>% html_nodes('.content-wrap') %>% 
                                 html_text() %>% 
                                 trimws() %>% 
                                 gsub('\\s+',' ', .)
                               
                               return(data.frame(t_list))
                             }))
  return(ret_df)
}

blueminds_articles_content <- get_articles_blueminds()

blueminds_articles_content <- merge(blueminds_articles_content, 
                                    blueminds_article_links, 
                                    by.x = "article_URL", 
                                    by.y = "article_link")

# Write out the list of articles & characteristics
write.csv(blueminds_articles_content, 
          file = 'blueminds_files/blueminds_articles_content.csv',
          fileEncoding = "utf-8")

