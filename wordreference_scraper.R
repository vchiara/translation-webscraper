library(tidyverse)
library(rvest)
library(stringr)


get_words <- function(nodes, class){
 
  names(nodes) <- nodes %>% html_attr("class")
  type <- names(nodes)==class
  
  word_list <- list()
  current_list <- list()

  i = 1
  while(i <= length(nodes)){
    
    if(type[i]){
      from <- nodes[i] %>% html_nodes('.FrWrd strong') %>% 
              html_text() %>% 
              str_remove_all("\u21D2")
     
       to <- nodes[i] %>% html_nodes('.ToWrd') %>% 
              html_nodes(xpath = 'text()') %>% 
              html_text() %>%
              str_remove_all("\u21D2")
      
      current_list$from <- c(current_list$from, from) 
      current_list$to <- c(current_list$to, to)
      
      if(i == length(nodes)){
        word_list[[length(word_list)+1]] <- current_list
      }
      
    } else if (length(current_list) > 0) {
      word_list[[length(word_list)+1]] <- current_list
      current_list <- list()
    }
    
    i = i + 1
  }
  
  return(word_list)
}


translate <- function(url){
  
  site <- read_html(iconv(url, to = "UTF-8"), encoding = "utf8")
  nodes <- site %>% html_nodes('table.WRD > tr')
  
  language_source <- site %>%
    html_node(xpath = '/html/head/meta[3]') %>%
    html_attr('content')
  
  if (language_source %in% c("en,es", "es,en")) {
    language_translation <- str_split(language_source, ",")[[1]][2]
    language_source <- str_split(language_source, ",")[[1]][1] 
  } else {
    lang <- site %>% html_node('#nav > a') %>% html_attr("href")
    language_source <- lang %>% str_sub(start = 2, end = -4)
    language_translation <- lang %>% str_sub(start = 4, end = -2)
  }
  
  assign("language_source", language_source, envir = .GlobalEnv)
  assign("language_translation", language_translation, envir = .GlobalEnv)
  
  if(is_empty(nodes)){
    script <- site %>%
      html_nodes("#articleHead > script") %>% 
      html_attr("src")
    
    suggestions <- read_html(script) %>%
      html_nodes("a") %>% 
      html_text()
    
    assign("suggestions", suggestions, envir = .GlobalEnv)
  }
  
  word_list_even <- get_words(nodes, "even")
  word_list_odd <- get_words(nodes, "odd")
  
  translation <- append(word_list_even, word_list_odd)
  
  assign("translation", translation, envir = .GlobalEnv)
}