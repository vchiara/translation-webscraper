library(dplyr)
library(rvest)
library(stringr)

translate <- function(url){
  site <- read_html(iconv(url, to = "UTF-8"), encoding = "utf8")
  
  from_word <- site %>%
    html_nodes('#articleWRD table.WRD:first-of-type .even .FrWrd strong') %>%
    html_text()
  
  to_word <- site %>% 
    html_nodes('#articleWRD table.WRD:first-of-type .even .ToWrd') %>%
    html_nodes(xpath = 'text()') %>%
    html_text()
  
  assign("to_word", to_word,  envir = .GlobalEnv)
  assign("from_word", from_word,  envir = .GlobalEnv)
}