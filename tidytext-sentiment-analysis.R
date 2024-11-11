# Load required packages
library(tidyverse)
library(tidytext)

prefix <- "radiosvoboda_" # "suspilne_"
input_file <- paste0(prefix, "article_texts.csv")
results_folder <- "results_sentiment-analysis"

# Source the Ukrainian stemmer script
source("ukrainian_stemmer.R")

# Read the custom lexicon file with semicolon separator
custom_lexicon <- read.csv("sentiment_ua.csv", sep = ";", stringsAsFactors = FALSE)

# Read the text data file
text_data <- read.csv(input_file, stringsAsFactors = FALSE)

# Inspect the data
print(head(custom_lexicon))
print(colnames(custom_lexicon))
print(head(text_data))
print(colnames(text_data))

# Transform the lexicon
custom_lexicon <- custom_lexicon %>%
  mutate(sentiment = case_when(
    pos_neg > 0 ~ "positive",
    pos_neg < 0 ~ "negative",
    pos_neg == 0 ~ "neutral"
  )) %>%
  select(word, sentiment)

# Preprocess text data
text_data <- text_data %>%
  mutate(document = row_number(),
         text = str_to_lower(text),  # Convert to lower case
         text = str_replace_all(text, "[^[:alpha:]]", " "))  # Remove special characters and numbers

# Tokenize and stem the text data
text_data <- text_data %>%
  unnest_tokens(word, text, drop = FALSE) %>%
  mutate(word = sapply(word, ukrainian_stemmer))

# Join the text data with the custom lexicon
sentiment_data <- text_data %>%
  inner_join(custom_lexicon, by = "word")

# Calculate sentiment scores for each row
sentiment_summary <- sentiment_data %>%
  group_by(document) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

results <- text_data %>%
  select(document, title) %>%
  distinct(document, title) %>%
  left_join(sentiment_summary, by = "document") %>%
  filter(!is.na(sentiment_score)) %>%
  select(title, negative, positive, sentiment_score)  # Only keep required columns

# Create results directory if it doesn't exist
if (!dir.exists(results_folder)) {
  dir.create(results_folder)
}

output_file <- paste0(results_folder, "/tidytext-sentiment-analysis_", prefix, ".csv")

# Save the results to a CSV file with the updated name
write.csv(results, output_file, row.names = FALSE)
