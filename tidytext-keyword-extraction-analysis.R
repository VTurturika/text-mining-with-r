# Install and load the required packages
install.packages("tm")
install.packages("tidytext")
install.packages("lubridate")
install.packages("ggplot2")

library(tm)
library(tidytext)
library(lubridate)
library(ggplot2)
library(dplyr) # Required for data manipulation

# Define file paths
file_path <- "article_texts.csv"
stopwords_file <- "stopwords_ua.txt"
stemmer_file <- "ukrainian_stemmer.R"

# Read data from CSV file
data <- read.csv(file_path, stringsAsFactors = FALSE)

# Display the first few rows of the data to verify the content
head(data)

# Extract the text column
texts <- data$text

# Read custom stop words
custom_stopwords <- scan(stopwords_file, what = "character", sep = "\n")

# Source the Ukrainian stemmer function
source(stemmer_file)

# Ensure the stemming function works with individual words
stem_words <- function(word) {
  ukrainian_stemmer(word)
}

# Preprocess text using `tm`
corpus <- VCorpus(VectorSource(texts))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, custom_stopwords)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(function(text) {
  words <- unlist(strsplit(text, " "))
  stemmed_words <- sapply(words, stem_words)
  paste(stemmed_words, collapse = " ")
}))

# Create a Document-Term Matrix (DTM)
dtm <- DocumentTermMatrix(corpus)
dtm_matrix <- as.matrix(dtm)

# Convert DTM to tidy format
tidy_dtm <- tidy(dtm)

# Calculate TF-IDF and get top 50 keywords
tf_idf <- tidy_dtm %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf)) %>%
  head(50)

# Display the top 50 keywords
cat("Top 50 keywords for all data:\n")
print(tf_idf)

# Create and display a bar chart of the top 50 keywords
create_chart <- function(keywords, title) {
  p <- ggplot(keywords, aes(x = reorder(term, tf_idf), y = tf_idf)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(tf_idf, 2)), hjust = -0.1) +
    coord_flip() +
    labs(title = title, x = "Terms", y = "TF-IDF Score") +
    theme_minimal()
  
  print(p)
}

create_chart(tf_idf, "Top 50 Keywords for All Data")
