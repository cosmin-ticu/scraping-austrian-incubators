rm(list = ls())
library(tidyverse)
library(rvest)
library(xml2)
library(data.table)
library(magrittr)

# Get links of all Impact Hub articles (they have 20 pages of content back to 2015)

get_links_ventury <- function(){

    ventury_news_page <- read_html("D:/OneDrive - Central European University/Courses/Spring_Term/Data Science 3/Term Project/ventury_files/webpage.html")
    
    t <- ventury_news_page %>% html_nodes('.inner-post')
    
    t_list <- list()
    
    t_list[['article_link']] <- t %>% 
      html_nodes('.title') %>%
      html_nodes('a') %>% html_attr('href')
    
    t_list[['img_link']] <- ifelse(grepl("<img", t %>% 
                                           html_nodes('.featured-thumb') %>%
                                           html_nodes('a')), t %>% 
                                     html_nodes('.featured-thumb') %>%
                                     html_nodes('img') %>% html_attr('src'),
                                   "no_image")
    
    print(paste("Currently scraping:","https://theventury.com/blog/"))
    
    return(data.frame(t_list))
}

ventury_article_links <- get_links_ventury()

ventury_article_links <- ventury_article_links[ventury_article_links$article_link != "https://theventury.com/blog/46825/",]

ventury_article_links <- ventury_article_links[!duplicated(ventury_article_links$article_link),]

# Download images outside of the scraping loop
count <- 1
for (i in ventury_article_links$img_link) {
  if(!is.na(i) & !http_error(i)){
    download.file(i,
                  destfile = paste0("ventury_files/ventury_img",
                                    count,'.png'),
                  mode = 'wb')
  }
  count <- count + 1
}

# Write out the list of article and image links
write.csv(ventury_article_links, file = 'ventury_files/ventury_article_links.csv')

ventury_article_url <- "https://theventury.com/blog/voice-assistants/"

get_articles_ventury <- function(){
  ret_df <- rbindlist(lapply(ventury_article_links[[1]], 
                             function(ventury_article_url){
                               t <- read_html(ventury_article_url)
                               
                               print(paste("Currently scraping:",ventury_article_url))
                               
                               t_list <- list()
                               
                               t_list[['creator']] <- 'TheVentury'
                               
                               t_list[['article_URL']] <- ventury_article_url
                               
                               t_list[['article_title']] <- t %>% 
                                 html_nodes('.banner-content') %>% 
                                 html_nodes('.title') %>% 
                                 html_text() %>% trimws()
                               
                               t_list[['content']] <- t %>% 
                                 html_node('.content-details') %>% 
                                 html_node('.content') %>% 
                                 html_text() %>%
                                 str_split(.,"Sign up to stay in the loop") %>% 
                                 extract2(1) %>%
                                 extract2(1) %>% 
                                 paste(., collapse = " ") %>% 
                                 trimws() %>% 
                                 gsub('\\s+',' ', .)
                               
                               t_list[['date']] <- t %>% 
                                 html_nodes('.banner-content') %>% 
                                 html_nodes(".category") %>%
                                 html_nodes('span') %>%
                                 extract2(1) %>% 
                                 html_text() %>% 
                                 trimws()
                               
                               return(data.frame(t_list))
                             }))
  return(ret_df)
}

ventury_articles_content <- get_articles_ventury()

ventury_articles_content <- merge(ventury_articles_content, 
                                  ventury_article_links, 
                                  by.x = "article_URL", 
                                  by.y = "article_link")

# Write out the list of articles & characteristics
write.csv(ventury_articles_content, 
          file = 'ventury_files/ventury_articles_content.csv',
          fileEncoding = "utf-8")

