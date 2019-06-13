library(dplyr)
library(rvest)

translate <- function(url){
  site <- read_html(url)
  
  from_word <- site %>%
    html_nodes('#articleWRD table.WRD:first-of-type .even .FrWrd strong') %>%
    html_text()
  
  to <- site %>% 
    html_nodes('#articleWRD table.WRD:first-of-type .even .ToWrd') %>%
    html_text()
  
  to_word <- strsplit(to, "\\w+:.*") %>% str_trim()
  
  assign("to_word", to_word,  envir = .GlobalEnv)
  assign("from_word", from_word,  envir = .GlobalEnv)
}