#Raw data importing in csv file
library(rvest)


categories_list <- list(
  Business = "https://www.thedailystar.net/business",
  Sports = "https://www.thedailystar.net/sports",
  Entertainment = "https://www.thedailystar.net/entertainment",
  LifeLiving = "https://www.thedailystar.net/life-living",
  Youth = "https://www.thedailystar.net/youth"
)


scrape_category <- function(base_url, category_name) {
  all_titles <- c()
  all_links <- c()
  page_number <- 0
  
  while (length(all_links) < 100) {
    page_url <- paste0(base_url, "?page=", page_number)
    webpage <- tryCatch(read_html(page_url), error = function(e) NULL)
    if (is.null(webpage)) break
    
    title_nodes <- html_nodes(webpage, ".card-content a")
    titles <- html_text(title_nodes)
    links <- html_attr(title_nodes, "href")
    full_links <- paste0("https://www.thedailystar.net", links)
    
    
    new_titles <- titles[!full_links %in% all_links]
    new_links <- full_links[!full_links %in% all_links]
    
    all_titles <- c(all_titles, new_titles)
    all_links <- c(all_links, new_links)
    
    page_number <- page_number + 1
    Sys.sleep(1)
  }
  
  
  all_titles <- all_titles[1:100]
  all_links <- all_links[1:100]
  
  get_description = function(link) {
    tryCatch({
      news_page = read_html(link)
      
     
      para = html_nodes(news_page, ".clearfix p")
      if (length(para) == 0) {
        para = html_nodes(news_page, ".field--name-body p")
      }
      if (length(para) == 0) {
        para = html_nodes(news_page, "article p")
      }
      
      para_text = html_text(para)
      if (length(para_text) == 0) {
        return(NA)
      }
      return(paste(para_text, collapse = " "))  # full description
    }, error = function(e) NA)
  }
  
  
  
  get_time <- function(link) {
    tryCatch({
      page <- read_html(link)
      times <- page %>% html_nodes(".color-iron") %>% html_text(trim = TRUE)
      full_time <- times[grepl("Last update on:|Published on:", times)][1]
      if (is.na(full_time) || full_time == "") {
        full_time <- times[times != ""][1]
      }
      return(full_time)
    }, error = function(e) NA)
  }
  
  descriptions = sapply(all_links, get_description)
  times = sapply(all_links, get_time)
  
  data.frame(
    category = category_name,
    news_link = all_links,
    title = all_titles,
    description = descriptions,
    time = times,
    stringsAsFactors = FALSE
  )
}


final_data <- do.call(rbind, lapply(names(categories_list), function(cat) {
  cat("Scraping", cat, "...\n")
  scrape_category(categories_list[[cat]], cat)
}))


write.csv(final_data, "ids_final_project_group_9_news_raw.csv", row.names = FALSE)
cat("Datas saved to 'ids_final_project_group_9_news_raw.csv'\n")

#text processing

library(tm)
library(textclean)     
library(textstem)      
library(tokenizers)   
library(hunspell)     
library(dplyr)


news_data <- read.csv("E:/DataScience/ids_final_project_group_9_news_raw.csv", stringsAsFactors = FALSE)


expand_contractions <- function(text) {
  replace_contraction(text)
}


handle_emojis <- function(text) {
  replace_emoji(text)
}




clean_text <- function(text) {
  text %>%
    tolower() %>%
    gsub("<.*?>", " ", .) %>%             
    gsub("[^a-z\\s]", " ", .) %>%            
    gsub("\\s+", " ", .) %>%                 
    trimws()
}


tokenize_text <- function(text) {
  unlist(tokenize_words(text))
}


remove_stopwords <- function(tokens) {
  tokens[!tokens %in% stopwords("en")]
}


stem_and_lemmatize <- function(tokens) {
  lemmatize_words(stem_strings(tokens))
}


spell_check <- function(text) {
  words <- unlist(strsplit(text, "\\s+"))
  corrected_words <- sapply(words, function(w) {
    if (!hunspell_check(w)) {
      sugg <- hunspell_suggest(w)[[1]]
      if (length(sugg) > 0) return(tolower(sugg[1]))
    }
    return(tolower(w))  
  })
  paste(corrected_words, collapse = " ")
}

process_text <- function(text_vector) {
  sapply(text_vector, function(text) {
    text %>%
      expand_contractions() %>%
      handle_emojis() %>%
      clean_text() %>%
      {
        tokens <- tokenize_text(.)
        tokens <- remove_stopwords(tokens)
        tokens <- stem_and_lemmatize(tokens)
        paste(tokens, collapse = " ")
      } %>%
      spell_check() %>%
      tolower()  
  }, USE.NAMES = FALSE)
}


news_data$processed_description <- process_text(news_data$description)


write.csv(news_data, "E:/DataScience/ids_final_project_group_9_news_clean.csv", row.names = FALSE)

cat("Text preprocessing is done. Saved as 'ids_final_project_group_9_news_clean.csv'\n")
