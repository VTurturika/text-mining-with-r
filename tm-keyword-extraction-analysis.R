# Install and load the required packages
library(tm)
library(SnowballC)
library(dplyr)
library(lubridate)
library(ggplot2)

# Define file paths
file_path <- "article_texts.csv"
stopwords_file <- "stopwords_ua.txt"
stemmer_file <- "ukrainian_stemmer.R"
output_prefix <- "results/tm_keyword_extractions"

# Read data from CSV file
data <- read.csv(file_path, stringsAsFactors = FALSE)

# Display the first few rows of the data to verify the content
head(data)

# Extract the text and date_published columns
texts <- data$text
dates <- data$date_published

# Convert dates to year
years <- year(ymd(dates))

# Read custom stop words
custom_stopwords <- scan(stopwords_file, what = "character", sep = "\n")

# Source the Ukrainian stemmer function
source(stemmer_file)

# Ensure the stemming function works with individual words
stem_words <- function(text) {
  words <- unlist(strsplit(text, " "))
  stemmed_words <- sapply(words, ukrainian_stemmer)
  paste(stemmed_words, collapse = " ")
}

# Preprocess each text
preprocess_text <- function(text) {
  text <- tolower(text)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- removeWords(text, custom_stopwords)
  text <- stripWhitespace(text)
  text <- stem_words(text)
  return(text)
}

# Apply preprocessing and group texts by year
data$processed_text <- sapply(texts, preprocess_text)
data$year <- years

# Function to perform TF-IDF and get top 50 keywords for each year
process_yearly_corpus <- function(yearly_data) {
  corpus <- VCorpus(VectorSource(yearly_data$processed_text))
  dtm <- DocumentTermMatrix(corpus)
  dtm_matrix <- as.matrix(dtm)
  
  term_frequency <- colSums(dtm_matrix)
  doc_count <- nrow(dtm_matrix)
  term_document_frequency <- colSums(dtm_matrix > 0)
  idf <- log(doc_count / term_document_frequency)
  
  tf_idf <- dtm_matrix
  for (i in 1:ncol(tf_idf)) {
    tf_idf[, i] <- tf_idf[, i] * idf[i]
  }
  
  tf_idf_scores <- colSums(tf_idf)
  sorted_keywords <- sort(tf_idf_scores, decreasing = TRUE)
  top_50_keywords <- head(sorted_keywords, 50)
  
  return(top_50_keywords)
}

# Create results directory if it doesn't exist
if (!dir.exists("results")) {
  dir.create("results")
}

# Function to create and save a bar chart
create_chart <- function(keywords, title, filename) {
  keywords_df <- data.frame(
    Term = names(keywords),
    Score = keywords
  )
  p <- ggplot(keywords_df, aes(x = reorder(Term, Score), y = Score)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(Score, 2)), hjust = -0.1) +
    coord_flip() +
    labs(title = title, x = "Terms", y = "TF-IDF Score") +
    theme_minimal()
  
  ggsave(filename, plot = p, width = 10, height = 8, units = "in", dpi = 300)
}

# Process all data and save results
all_data_keywords <- process_yearly_corpus(data)
all_data_df <- data.frame(
  Term = names(all_data_keywords),
  Score = all_data_keywords
)
write.csv(all_data_df, file = paste0(output_prefix, "_all_data.csv"), row.names = FALSE)
cat("Top 50 keywords for all data:\n")
print(all_data_keywords)
cat("\n")
create_chart(all_data_keywords, "Top 50 Keywords for All Data", paste0(output_prefix, "_all_data.jpg"))

# Process data by each year and save results
years <- unique(data$year)
for (yr in years) {
  yearly_data <- filter(data, year == yr)
  top_50_keywords <- process_yearly_corpus(yearly_data)
  yearly_data_df <- data.frame(
    Term = names(top_50_keywords),
    Score = top_50_keywords
  )
  write.csv(yearly_data_df, file = paste0(output_prefix, "_", yr, ".csv"), row.names = FALSE)
  cat(paste("Top 50 keywords for the year", yr, ":\n"))
  print(top_50_keywords)
  cat("\n")
  create_chart(top_50_keywords, paste("Top 50 Keywords for the Year", yr), paste0(output_prefix, "_", yr, ".jpg"))
}

# Display message confirming the completion
print("Keyword extraction and chart creation completed for all data and each year.")
