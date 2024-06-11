# Load required packages
library(tm)
library(stringi)
library(dplyr)
library(ggplot2)

# Source the Ukrainian stemmer
source("ukrainian_stemmer.R")

# Read the CSV file
text_data <- read.csv("article_texts.csv", stringsAsFactors = FALSE)

# Extract the text and date_published columns
texts <- text_data$text
dates <- text_data$date_published

# Read stopwords from file
stopwords_ua <- readLines("stopwords_ua.txt", encoding = "UTF-8")

# Create a data frame with text data and years
text_df <- data.frame(text = texts, year = substr(dates, 1, 4), stringsAsFactors = FALSE)

# Function to preprocess text by removing special characters and numbers
preprocess_text <- function(text) {
  text <- tolower(text)                            # Convert to lowercase
  text <- removePunctuation(text)                  # Remove punctuation
  text <- stri_replace_all_regex(text, "[^\\p{L}\\s]", "")  # Remove special characters and numbers
  words <- unlist(strsplit(text, "\\s+"))          # Split text into words
  words <- words[!words %in% stopwords_ua]         # Remove stopwords
  stemmed_words <- sapply(words, ukrainian_stemmer) # Apply stemming
  return(paste(stemmed_words, collapse = " "))     # Reconstruct text
}

# Apply preprocessing to all texts
text_df <- text_df %>%
  mutate(text = sapply(text, preprocess_text))

# Ensure the results folder exists
if (!dir.exists("results")) {
  dir.create("results")
}

# Function to process text for a specific dataset (either overall or per year)
process_data <- function(data, identifier) {
  # Create a corpus from the processed texts
  corpus <- Corpus(VectorSource(data$text))
  
  # Create a document-term matrix
  dtm <- DocumentTermMatrix(corpus)
  
  # Calculate word frequencies
  word_freqs <- colSums(as.matrix(dtm))
  word_freqs <- sort(word_freqs, decreasing = TRUE)
  word_freq_df <- data.frame(word = names(word_freqs), freq = word_freqs)
  
  # Save the word frequencies to a CSV file
  csv_file <- paste0("results/tm_word_frequencies_", identifier, ".csv")
  write.csv(word_freq_df, csv_file, row.names = FALSE)
  
  # Select the top 10 words
  top_10_words <- head(word_freq_df, 10)
  
  # Plotting the top 10 words using ggplot2 with absolute values on columns
  p <- ggplot(top_10_words, aes(x = reorder(word, -freq), y = freq)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = freq), vjust = -0.5) +
    theme_minimal() +
    xlab("Words") + 
    ylab("Frequency") +
    ggtitle(paste("Top 10 Word Frequencies for", identifier)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save the plot as a JPG file
  jpg_file <- paste0("results/tm_top_10_word_frequencies_", identifier, ".jpg")
  ggsave(jpg_file, plot = p, width = 10, height = 8)
}

# Process overall data
process_data(text_df, "all_data")

# Group data by year and process each year
text_df %>%
  group_by(year) %>%
  group_walk(~ process_data(.x, .y$year))
