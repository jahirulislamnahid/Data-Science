
library(readr)
library(tm)
library(topicmodels)
library(dplyr)
library(stringr)


data <- read_csv("E:/DataScience/ids_final_project_group_9_news_clean.csv")


corpus <- VCorpus(VectorSource(data$processed_description))


custom_stopwords <- c(stopwords("en"), "will", "can", "also", "say", "make", "get")

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, custom_stopwords)
corpus <- tm_map(corpus, stripWhitespace)



dtm <- DocumentTermMatrix(corpus)


dtm <- removeSparseTerms(dtm, 0.99)


row_totals <- apply(dtm, 1, sum)
dtm <- dtm[row_totals > 0, ]


set.seed(123)
lda_model <- LDA(dtm, k = 5, control = list(seed = 123))


top_terms <- terms(lda_model, 10)
print("Top terms in each topic:")
print(top_terms)


interpret_topic <- function(top_terms_vec) {
  keywords <- tolower(top_terms_vec)
  
  categories <- list(
    "Economic & Business News" = c(
      "market", "stock", "business", "economy", "growth", "price", "trade", "bank", "dollar", "investment",
      "percent", "rate", "corer", "high", "tax", "inflation", "finance", "budget", "profit", "loss"
    ),
    
    "Sports News" = c(
      "match", "player", "score", "team", "game", "win", "coach", "tournament", "goal", "league",
      "final", "season", "championship", "cricket", "football", "cup", "run", "bat", "ball"
    ),
    
    "Entertainment News" = c(
      "movie", "actor", "film", "music", "celebrity", "show", "award", "drama", "director", "release",
      "song", "feature", "performance", "cinema", "scene", "album", "entertainment", "star", "role"
    ),
    
    "Lifestyle & Living" = c(
      "health", "life", "living", "environment", "travel", "food", "family", "fashion", "fitness", "climate",
      "feel", "home", "habit", "culture", "experience", "emotion", "beauty", "diet", "wellness"
    ),
    
    "Youth & Education" = c(
      "youth", "education", "student", "career", "social", "event", "community", "school", "university", "teacher",
      "research", "study", "intern", "exam", "campus", "degree", "academic", "learn", "graduate"
    )
  )
  
  
  match_counts <- sapply(categories, function(keywords_list) {
    sum(keywords %in% keywords_list)
  })
  
  best_category <- names(which.max(match_counts))
  return(best_category)
}


for (i in 1:5) {
  terms_vec <- top_terms[, i]
  interpretation <- interpret_topic(terms_vec)
  
  cat(paste0("\nTopic ", i, " contains: [", paste(terms_vec, collapse = ", "), "]\n"))
  cat(paste0("Interpreted as: *", interpretation, "*\n"))
  cat(strrep("-", 50), "\n")
}
