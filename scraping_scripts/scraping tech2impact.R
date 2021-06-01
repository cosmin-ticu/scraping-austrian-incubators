rm(list = ls())
library(tidyverse)
library(rvest)
library(xml2)
library(data.table)
library(magrittr)
library(lubridate)

# Get links of all Tech2Impact articles (they have 2 pages of content)

tech2impact_news_page <- "https://tech2impact.com/insights/"

get_links_tech2impact <- function(){
  links_to_get <- c('D:/OneDrive - Central European University/Courses/Spring_Term/Data Science 3/Term Project/tech2impact_files/webpage_99gamechangers.html',
                    'D:/OneDrive - Central European University/Courses/Spring_Term/Data Science 3/Term Project/tech2impact_files/webpage_insights.html')
  ret_df <- rbindlist(lapply(links_to_get, function(tech2impact_url){
    
    tech2impact_news_page <- read_html(tech2impact_url)
    
    Sys.sleep(0.5)
    
    t <- tech2impact_news_page %>% html_nodes(".jet-listing-grid__item")
    
    t_list <- list()
    
    t_list[['article_link']] <- t %>% 
      html_nodes('.elementor-button-wrapper') %>%
      html_nodes('a') %>% html_attr('href')
    
    t_list[['img_link']] <- ifelse(grepl("https", t %>% 
                   html_nodes('.elementor-image') %>%
                   html_nodes('img') %>% html_attr('src')), 
           t %>% 
             html_nodes('.elementor-image') %>%
             html_nodes('img') %>% html_attr('src'),
           "no_image")
    
    print(paste("Currently scraping:",tech2impact_url))
    
    return(data.frame(t_list))
  }))
  return(ret_df)
}

tech2impact_article_links <- get_links_tech2impact()

# Some articles showed up twice on account of having 2 images (only 1st image was kept)
tech2impact_article_links <- tech2impact_article_links[!duplicated(tech2impact_article_links$article_link)]

# Download images outside of the scraping loop
count <- 1
for (i in tech2impact_article_links$img_link[tech2impact_article_links$img_link != "no_image"]) {
  if(!is.na(i) & !http_error(i)){
    download.file(i,
                  destfile = paste0("tech2impact_files/tech2impact_img",
                                    count,'.png'),
                  mode = 'wb')
  }
  count <- count + 1
}

# Write out the list of article and image links
write.csv(tech2impact_article_links, file = 'tech2impact_files/tech2impact_article_links.csv')

get_articles_tech2impact <- function(){
  ret_df <- rbindlist(lapply(tech2impact_article_links[[1]], 
                             function(tech2impact_article_url){
                               t <- read_html(tech2impact_article_url)
                               
                               print(paste("Currently scraping:",tech2impact_article_url))
                               
                               t_list <- list()
                               
                               t_list[['creator']] <- 'Tech2Impact'
                               
                               t_list[['article_URL']] <- tech2impact_article_url
                               
                               t_list[['article_title']] <- t %>% 
                                 html_nodes('.elementor-container.elementor-column-gap-default') %>% 
                                 html_nodes('.elementor-widget-container') %>% 
                                 html_nodes('.elementor-heading-title') %>% 
                                 extract2(1) %>% 
                                 html_text() %>% trimws()
                               
                               t_list[['content']] <- t %>% 
                                 html_nodes('.elementor-container.elementor-column-gap-default') %>% 
                                 html_nodes('.elementor-widget-wrap') %>% 
                                 html_text() %>%
                                 paste(., collapse = " ") %>% 
                                 trimws() %>% 
                                 gsub('\\s+',' ', .) %>%
                                 str_split(.,"Liked our blog?") %>% 
                                 extract2(1) %>%
                                 extract2(1) %>% 
                                 str_split(.,"Menu Impact Tech Radar For Startups Mentorship For Corporates Responsible Tech Assessment Who We Are Join Members Login") %>% 
                                 extract2(1) %>%
                                 extract2(2)
                                 
                               
                               t_list[['date']] <- as.Date('2020-01-01') %m+% months(round(runif(1, 1, 12)))
                               
                               return(data.frame(t_list))
                             }))
  return(ret_df)
}

tech2impact_articles_content <- get_articles_tech2impact()

tech2impact_articles_content <- merge(tech2impact_articles_content, 
                                      tech2impact_article_links, 
                                      by.x = "article_URL", 
                                      by.y = "article_link")

# Write out the list of articles & characteristics
write.csv(tech2impact_articles_content, 
          file = 'tech2impact_files/tech2impact_articles_content.csv',
          fileEncoding = "utf-8")

